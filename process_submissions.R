## Send from submissions to forecasts
minioclient::mc_alias_set("efi", "data.ecoforecast.org", "", "")
minioclient::mc_cp(from = paste0("efi/spat4cast-submissions/", theme),
                   to = paste0("efi/spat4cast-forecasts/", theme),
                   recursive = TRUE)