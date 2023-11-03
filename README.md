# modis-lai-forecast
project team: John Smith, David Durden,  Emma Mendelsohn, Carl Boettiger, EFI Cyberinfrastructure and Methods and Tools working groups

This repo holds code for a spatially explicit forecasting challenge pipeline to benchmark spatial models using MODIS leaf index data. In this example we focus on locations of wildfire burns and recovery.

- 🗺️ **[Example notebook](https://cboettig.github.io/modis-lai-forecast/spatial_forecast_example.html)**
- :octocat: [source](https://github.com/cboettig/modis-lai-forecast/blob/main/spatial_forecast_example.qmd)

## Workflow overview
![efi-spatial (3)](https://github.com/eco4cast/modis-lai-forecast/assets/16726030/8e9b7eb5-62ca-4a7f-9df1-a5c09bd569b2)


## Site selection
Our goal is to select locations across a variety of environments and burn conditions. Currently we have two sites selected from Monitoring Trends in Burn Severity ([MTBS](https://www.mtbs.gov/)). These shapefiles are available in the `/shp` directory.
- California August complex fire
- Colorado East Troublesome

## Functions

Functions are stored in the `R/` directory.

- `fire_bbox()` reads in a fire boundary shapefile and determines a bounding box for grabbing MODIS data with a padding option. 
- `ingest_planetary_data()` downloads data from Microsoft planetary comuputer and returns a `gdalcube` data cube proxy object. 
- `create_target_file()` subsets the data cube, pulls data for a given data and serializes target geotiff to disk.  
- `spat_climatology()` creates climatology predictions and serializes prediction geotiff to disk. Predictions are created using an ensemble of historical data within a given month. If historical data is missing, values are treated as `NA` and bootstrap re-sampling is performed using previous monthly data.
- `scoring_spat_ensemble()` assigns CRPS (Continuous Ranked Probability Scores) and Logarithmic Scores for a given target file and ensemble forecast. Serializes scored geotiff to disk.
- `na_bootstrap_fun()` is used internally for re-sampling during creation of climatological forecasts. The function takes a vector `x` of (possibly missing) data and fills `NA` values using a bootstrap re-sampling of non-`NA` values.

## Environment
This project uses `renv` for package management. Use `renv::restore()` to load project packages.

## Next steps
- Ingest additional fire sites. Potential locations
    - NEON GRSM: https://www.neonscience.org/
    - NEON SOAP: https://www.neonscience.org/field-sites/soap 
    - Arizona rapid burn/recovery
    - Eastern canada fires
- Ingest addition data streams (e.g., burn intensity from MTBS)
- Deployment for submissions

