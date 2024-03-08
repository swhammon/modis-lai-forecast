#' Score spatial parametric forecast using CRPS and Logarithmic Score
#'
#' @param fc_dir character; directory that geotiff ensemble forecasts are stored
#' @param target_dir character; directory that target geotiff is stored
#' @param scores_dir character; directory to store geotiff files of scores. If the specified directory does not exist, it is created
#' @return character; directory that geotiff score files are written to.
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
#' # Generate targets dir/files ------------------------------------------------------------
#' target_forecast_dir <- create_target_file(cuberast = raster_cube,
#'                                           date = '2023-06-22',
#'                                           dir = 'targets',
#'                                           mask = fire_box$maskLayer)
#' # Score ----------------------------------------------------------------
#' scored_forecast_dir <- scoring_spat_ensemble(fc_dir = ensemble_forecast_dir,
#'                                              target_dir = target_forecast_dir,
#'                                              scores_dir = 'scores')
#' @export
#'
#library(tidyverse)
#library(raster)
#library(tiff)
#library(tmvtnorm)
#library(score4cast)
#library(stringr)
#library(scoringRules)


spatial_score <- function(fc_dir, target_dir, scores_dir){

  target <- raster(paste0(target_dir,"/",list.files(path = target_dir)[1]))

  family = str_split_1(list.files(path = fc_dir)[1], "_")[1]

  if( family == "lognormal"){
    params = c("mu", "sigma")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_lnorm(getValues(target),
                             getValues(param_tiffs$mu),
                             getValues(param_tiffs$sigma))
    logs_scores = logs_lnorm(getValues(target),
                             getValues(param_tiffs$mu),
                             getValues(param_tiffs$sigma))
  }

  if( family == "normal"){
    params = c("mu", "sigma")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_norm(getValues(target),
                            getValues(param_tiffs$mu),
                            getValues(param_tiffs$sigma))
    logs_scores = logs_norm(getValues(target),
                            getValues(param_tiffs$mu),
                            getValues(param_tiffs$sigma))
  }

  if( family == "bernoulli"){
    params = c("prob")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_binom(getValues(target),
                             getValues(param_tiffs$prob))
    logs_scores = logs_binom(getValues(target),
                             getValues(param_tiffs$prob))
  }

  if( family == "beta"){
    params = c("shape1", "shape2")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_beta(getValues(target),
                            getValues(param_tiffs$shape1),
                            getValues(param_tiffs$shape2))
    logs_scores = logs_beta(getValues(target),
                            getValues(param_tiffs$shape1),
                            getValues(param_tiffs$shape2))
  }

  if( family == "uniform"){
    params = c("min", "max")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_unif(getValues(target),
                            getValues(param_tiffs$min),
                            getValues(param_tiffs$max))
    logs_scores = logs_unif(getValues(target),
                            getValues(param_tiffs$min),
                            getValues(param_tiffs$max))
  }

  if( family == "gamma"){
    params = c("shape", "rate")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_gamma(getValues(target),
                            getValues(param_tiffs$shape),
                            getValues(param_tiffs$rate))
    logs_scores = logs_gamma(getValues(target),
                            getValues(param_tiffs$shape),
                            getValues(param_tiffs$rate))
  }

  if( family == "logistic"){
    params = c("location", "scale")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_logis(getValues(target),
                            getValues(param_tiffs$location),
                            getValues(param_tiffs$scale))
    logs_scores = logs_logis(getValues(target),
                            getValues(param_tiffs$location),
                            getValues(param_tiffs$scale))
  }

  if( family == "exponential"){
    params = c("rate")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_exp(getValues(target),
                            getValues(param_tiffs$rate))
    logs_scores = logs_exp(getValues(target),
                            getValues(param_tiffs$rate))
  }

  if( family == "poisson"){
    params = c("lambda")
    param_tiffs <- vector("list", length = length(params))
    names(param_tiffs) <- params
    for (i in 1:length(params)){
      ind <- grep(x = list.files(path = fc_dir), pattern = params[i])
      param_tiffs[[params[i]]] <- raster(paste0(fc_dir,"/",list.files(path = fc_dir)[ind]))
    }

    crps_scores = crps_pois(getValues(target),
                            getValues(param_tiffs$lambda))
    logs_scores = logs_pois(getValues(target),
                            getValues(param_tiffs$lambda))
  }



  crps_raster <-
    raster::raster(extent(0, 150, 0, 100), # xmin, xmax, ymin, ymax
                   res = 50, # resolution of 50 meters
                   crs = 31370) %>%
    setValues(crps_scores)

  logs_raster <-
    raster::raster(extent(0, 150, 0, 100), # xmin, xmax, ymin, ymax
                   res = 50, # resolution of 50 meters
                   crs = 31370) %>%
    setValues(logs_scores)

  raster::writeRaster(crps_raster,
                     filename = paste0(scores_dir, "/crps_scores.tif"),
                     overwrite=TRUE)
  raster::writeRaster(logs_raster,
                     filename = paste0(scores_dir, '/logs_scores.tif'),
                     overwrite=TRUE)


  return(scores_dir)
}





