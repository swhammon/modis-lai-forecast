
spat4cast_submit <- function(dir, theme = "lai_recovery") {
  # upload anonymously to submissions bucket
  minioclient::mc_alias_set("efi", "data.ecoforecast.org", "", "")
  minioclient::mc_cp(dir, 
                     paste0("efi/spat4cast-submissions/", theme),
                     recursive = TRUE)
}