#' Generate spatial targets
#'
#' @param dir A directory of .tif files for scoring
#' @param site_id The side ID of the area being forecasted. Currently "august_complex" is the only option.
#'
#' @return message from minio submission
#' @export
#'
#' @examples spat4cast_submit(dir = "targets", site_id = "august_complex")



spatial_targets <- function(
    dir = "targets", 
    site_id = c("august_complex")[1],
    dx = 0.1,
    dy = 0.1
    )
{

print(paste0("Running Creating Terrestrial Targets at ", Sys.time()))

library(here)
library(sf)
library(lubridate)
library(gdalcubes)
library(rstac)
library(stars)


#Source functions
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

#Create fire bounding box
fire_box <- fire_bbox(fire = site_id, pad_box = TRUE)

#Target date
date <- lubridate::floor_date(as.Date(Sys.time()), "month") #first day of the month

# Ingest data ------------------------------------------------------------
gdalcubes::gdalcubes_options(parallel=TRUE)

# use ingest_planetary_data function to extract raster cube for fire bounding box between Jan 1 2002 and July 1 2023.
raster_cube <- ingest_planetary_data(start_date = date - months(1), 
                                     end_date = date, 
                                     box = fire_box$bbox,
                                     srs = "EPSG:4326",
                                     dx = dx, 
                                     dy = dy, 
                                     dt = "P30D",
                                     collection = "modis-15A2H-061",
                                     asset_name = "Lai_500m")



# create target file
  target <- create_target_file(cuberast = raster_cube,
                               site_id = site_id,
                               date = as.character(date),
                             dir = tempdir(),
                             bucket = "efi/spat4cast-targets",
                             mask = fire_box$maskLayer)
                             
                             
} #End of function