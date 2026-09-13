library(tidyverse)

download_worldclim_climatology <- function(v_var = c("temp", "prcp"), worldclim_dir = "alldata/input/WorldClim/climatology/") {
  # https://www.worldclim.org/data/worldclim21.html

  for (var in v_var) {
    if (var == "temp") {
      worldclim_var <- "tavg"
    }
    if (var == "prcp") {
      worldclim_var <- "prec"
    }

    # download WorldClim data
    worldclim_url <- str_c("https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_", worldclim_var, ".zip")
    worldclim_file <- str_c("wc2.1_30s_", worldclim_var)

    if (list.files(worldclim_dir, worldclim_file) %>% length() == 0) {
      options(timeout = max(6000, getOption("timeout")))
      worldclim_file_zip <- str_c(worldclim_dir, worldclim_file, ".zip")
      download.file(worldclim_url, worldclim_file_zip, mode = "wb")
      unzip(worldclim_file_zip, exdir = str_c(worldclim_dir, worldclim_file))
      file.remove(worldclim_file_zip)
    }
  }
}

download_worldclim_climatology()

# download_worldclim_annual()
