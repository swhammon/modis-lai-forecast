#' Create target geotiff
#'
#' @param cuberast object of class cube; raster cube to generate target file on
#' @param dir character; directory that target geotiff is stored
#' @param date character; date to create target geotiff for
#' @param mask sf object; optional, mask polygon to use
#' @return character; directory that geotiff target file is written to.
#' @examples 
#' # Bounding box ------------------------------------------------------------
#' # pull box, mask
#' fire_box <- fire_bbox(fire = "august_complex", pad_box = TRUE)
#' # Ingest data ------------------------------------------------------------
#' raster_cube <- ingest_planetary_data(start_date = "2002-01-01", 
#'                                      end_date = "2023-07-01", 
#'                                      box = fire_box$bbox)
#' # Generate targets dir/files ------------------------------------------------------------
#' target_forecast_dir <- create_target_file(cuberast = raster_cube, 
#'                                           date = '2023-06-22',
#'                                           dir = 'targets',
#'                                           mask = fire_box$maskLayer)

create_target_file <- function(cuberast, date, dir=NULL, mask = NULL){
  
  target <- cuberast %>%
    gdalcubes::slice_time(date) 
  
  if (!is.null(mask)) {
    target <- target %>%
      gdalcubes::filter_geom(geom = mask,
                             srs = "EPSG:4326")
  }

  if (!is.null(dir)) {
    
    # needed to write geotif to VSI
    Sys.setenv("CPL_VSIL_USE_TEMP_FILE_FOR_RANDOM_WRITE"="YES") 
    
    # doesn't take VSI yet; so convert to stars first instead:
    # write_tif(target, dir, "lai_recovery_target_") 
    
    target %>% 
      stars::st_as_stars() %>%
      write_stars(glue::glue("{dir}/lai_recovery-target-{date}.tif"))
  }
  
  invisible(target)
}
