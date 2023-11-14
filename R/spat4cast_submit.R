#' Submit spatial forecast
#'
#' @param dir A directory of .tif files for scoring
#' @param model_id A unique ID representing your group and model
#' @param variable The data source being forecasted. Currently "lai_recovery" is the only option.
#' @param site_id The side ID of the area being forecasted. Currently "august_complex" is the only option.
#'
#' @return
#' @export
#'
#' @examples
exported <- function(x) {
  x + 1
}
spat4cast_submit <- function(dir, 
                             model_id,
                             variable = c("lai_recovery"), 
                             site_id = c("august_complex")) {
  
  # Check variable and site_id
  variable <- match.arg(variable)
  site_id <- match.arg(site_id)
  
  # Check file type
  submitted_files <- list.files(dir, full = TRUE)
  assertthat::assert_that(n_distinct(tools::file_ext(submitted_files)) == 1, msg = "Error: One or more file extension in the submitted directory is not 'tif'. Please check file formats.")
  assertthat::assert_that(unique(tools::file_ext(submitted_files)) == "tif", msg = "Error: The file extension in the submitted directory is not 'tif'. Please check file formats.")
  
  # Check metadata for files
  ## can we use the `variable` and `site_id` arguments to pull a template target file for comparison?
  ## if we are to enforce these attributes, then we should modify `ingest_planetary_data` to remove dy, dx, dt, srs arguments (and have them enforced according to site_id)
  submitted_files_rast <- rast(submitted_files)
  res(submitted_files_rast)
  origin(submitted_files_rast)
  ext(submitted_files_rast)
  crs(submitted_files_rast)
  
  # Upload anonymously to submissions bucket, using hive partitioning for file structure
  out <- glue::glue("efi/spat4cast-submissions/variable={variable}/site_id={site_id}/model_id={model_id}") 
  
  minioclient::mc_alias_set("efi", "data.ecoforecast.org", access_key = "", secret_key = "") # keys not needed for public bucket submission
  submit <- minioclient::mc_cp(dir, out, recursive = TRUE)
  
  # Return submit message (or maybe something more succinct?)
  return(submit)
  
}