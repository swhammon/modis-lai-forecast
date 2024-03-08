#' Build spatial-temporal ICAR forecast, currently WIP
#'
#' @param cuberast object of class cube; raster cube to generate forecasts on
#' @param date date as character format yyyy-mm-dd; date to generate forecast
#' @param dir character; directory to store geotiff files. If the specified directory does not exist, it is created
#' @param ensemble logical; whether or not to use weighted bootstrap resampling to generate an ensemble
#' @param num_ensembles integer; number of ensemble GeoTiffs to create
#' @param niter integer; number of MCMC iterations to perform. Defaults to 20,000
#' @param burn integer; number of MCMC iterations discarded to burn-in. Defaults to 5,000
#' @param inits list; optional argument to give init values for MCMC. Defaults to NULL
#' @return character; directory that geotiff files are written to.
#' @examples
#' # Bounding box ------------------------------------------------------------
#' # pull box, mask
#' fire_box <- fire_bbox(fire = "august_complex", pad_box = TRUE)
#' # Ingest data ------------------------------------------------------------
#' raster_cube <- ingest_planetary_data(start_date = "2002-01-01",
#'                                      end_date = "2023-07-01",
#'                                      box = fire_box$bbox)
#' # Generate targets dir/files ------------------------------------------------------------
#' # Forecast ----------------------------------------------------------------
#' ensemble_forecast_dir <- spat_climatology(cuberast = raster_cube,
#'                                           date = '2023-06-22',
#'                                           dir = 'climatology')
#' @export
#'

spat_icarrw <- function(cuberast, date, dir, ensemble = TRUE, num_ensembles = 50,
                        niter = 20000, burn = 5000, inits = NULL){
  ## extract dimensions from cuberast input
  d <- gdalcubes::dimension_values(cuberast)

  ## create adjacency matrix
  adj_w <- as.matrix(spatstat.sparse::gridadjacencymatrix(dim = c(d$x,d$y)) + 0)
  ## convert adjacency matrix to carAdjacency matrix for nimble
  car_mat <- nimble::as.carAdjacency(adj_w)

  ## add on NAs to end on matrix for nimble to forecast
  nimble_mat <- rbind(nimble_mat, rep(NA, d$x * d$y))

  ## start by grabbing first row
  y0 <- as.numeric(nimble_mat[1,])

  ## repeat until all NAs are filled in by
  ## repeated nearest neighbor imputation
  while (any(is.na(y0))){
    for (i in 1:length(y0)){
      if (is.na(y0[i])){
        y0[i] <- mean(y0[which(adj_w[i,] == 1)], na.rm = TRUE)
      }
      if (is.nan(y0[i])){
        y0[i] <- NA
      }
    }
  }
  ## spatial temporal random walk nimble code
  spat_icarrw_code <- nimbleCode({
    tau ~ dgamma(0.001, 0.001)
    sigma ~ dgamma(0.001, 0.001)
    for (i in 1:T) {
      s[1:N, i] ~ dcar_normal(adj[1:L], weights[1:L], num[1:N], 1/tau)
    }
    for (i in 1:N){
      y_mean[i,1] <- y0[i] + s[i,1]
      y[i,1] ~ dnorm(y_mean[i,1], sd = sigma)
    }
    for(i in 1:N) {
      for (t in 2:T){
        y_mean[i,t] <- y[i,t-1] + s[i,t]
        y[i,t] ~ dnorm(y_mean[i,t], sd = sigma)
      }
    }
  })
  ## constants list to be passed to nimble
  constants <- list(N = ncol(nimble_mat), L = length(car_mat$adj), num = car_mat$num, weights = car_mat$weights,
                    adj = car_mat$adj, T = nrow(nimble_mat), y0 = y0)

  ## data list to be passed to nimble
  data <- list(y = t(nimble_mat))

  ## check for inits argument; confirm that proper format is used
  if (is.null(inits)){
    ## default initial values if none are provided
    inits <- list(tau = .25, sigma = .05, s = matrix(0, nrow = constants$N, ncol = constants$T))
  } else{
    if (!is.list(inits)) stop('Argument inits expected to be passed as a list')
    if (!all(sort(names(inits)) == c('s', 'sigma', 'tau'))) stop('Argument inits must have names "s", "sigma", "tau".')
  }

  ## create nimbleModel
  Rmodel <- nimbleModel(code, constants, data, inits)

  ## configure model, enable WAIC
  mcmcConf <- configureMCMC(Rmodel, enableWAIC = TRUE)

  ## add y to list of monitored variables
  mcmcConf$addMonitors("y")

  ## build MCMC
  Rmcmc <- buildMCMC(mcmcConf)

  ## create compiled versions in C
  Cmodel <- compileNimble(Rmodel)
  Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

  ## run mcmc
  Cmcmc$run(niter)

  ## extract samples as matrix
  samples <- as.matrix(Cmcmc$mvSamples)

  ## set column names of samples
  param_colnames <- colnames(samples)[colnames(samples) %in% c('tau', 'sigma', paste0('y[', 1:(d$x * d$y), ', ',nrow(nimble_mat), ']'))]

  ## extract post burn-in samples
  postburn_samples <- samples[burn:niter,param_colnames]

  ## create directory
  dir.create(dir, FALSE)

  ## generate ensemble using density-weighted bootstrap resample with
  ## number of ensembles equal to num_ensembles
  ensemble_mat <- matrix(NA, nrow = num_ensembles, ncol = ncol(nimble_mat))
  for (j in 1:ncol(nimble_mat)){
    dens_approx <- density(postburn_samples[, paste0('y[', i, ', ', nrow(nimble_mat), ']')])
    ensemble_mat[,j] <- sample(dens_approx$x, size = num_ensembles, replace = TRUE, prob = dens_approx$y/sum(dens_approx$y))
  }

  return(dir)
}
