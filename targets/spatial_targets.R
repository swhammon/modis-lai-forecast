
library(here)
library(sf)
library(lubridate)
library(gdalcubes)
library(rstac)
library(stars)

#Source functions
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)
# devtools::load_all()

dir = "targets"
site_id = c("august_complex")[1]
dt = "P1M"
dx = 0.1
dy = 0.1

print(paste0("Running Creating Terrestrial Targets at ", Sys.time()))



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
                                     dt = dt,
                                     collection = "modis-15A2H-061",
                                     asset_name = "Lai_500m")



# create target file
target <- create_target_file(cuberast = raster_cube,
                             site_id = site_id,
                             date = as.character(date),
                             dir = tempdir(),
                             bucket = "efi/spat4cast-targets",
                             mask = fire_box$maskLayer,
                             dt = dt,
                             var = "LAI_modis")
                             
                             
