
spat4cast_submit <- function(dir, theme = "lai_recovery", fire = "august_complex") {
  
  submitted_files <- list.files(dir, full = TRUE)
  
  # Check file type
  assertthat::assert_that(n_distinct(tools::file_ext(submitted_files)) == 1)
  assertthat::assert_that(unique(tools::file_ext(submitted_files)) == "tif", msg = "Error: The file extension in the directory is not 'tif'. Please check the file format.")
  
  # Check bounding box for files
  ## can we use the `fire` argument to pull a template target file for comparison?
  ## if we are to enforce these attributes, then we should modify `ingest_planetary_data` to remove dy, dx, dt, srs arguments (and have them enforced according to the fire of intered)
  res(rast(submitted_files))
  origin(rast(submitted_files))
  ext(rast(submitted_files))

  ## Upload anonymously to submissions bucket
  minioclient::mc_alias_set("efi", "data.ecoforecast.org", "", "")
  minioclient::mc_cp(dir, 
                     paste0("efi/spat4cast-submissions/", theme, "/", fire), # should we have separate buckets for each fire?
                     recursive = TRUE)
}