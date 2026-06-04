download_terraclim <- function(v_var = c("temp", "prcp")) {
  library(terra)

  for (var in v_var) {
    if (var == "temp") {
      worldclim_var <- "tavg"
    }
    if (var == "prcp") {
      worldclim_var <- "prec"
    }

    # download WorldClim data
    #BRI - Might need to change world_var to worldclim_var in lines 14 and 16
    worldclim_url <- str_c("https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_", world_var, ".zip")
    worldclim_dir <- "alldata/input/climate/"
    worldclim_file <- str_c("wc2.1_30s_", world_var)

    if (list.files(worldclim_dir, worldclim_file) %>% length() == 0) {
      options(timeout = max(600, getOption("timeout")))
      worldclim_file_zip <- str_c(worldclim_dir, worldclim_file, ".zip")
      download.file(worldclim_url, worldclim_file_zip, mode = "wb")
      unzip(worldclim_file_zip, exdir = str_c(worldclim_dir, worldclim_file))
      file.remove(worldclim_file_zip)
    }

    # read WorldClim data
    worldclim_files <- list.files(
      str_c(worldclim_dir, worldclim_file),
      pattern = ".tif$",
      full.names = TRUE
    )
    # read all rasters and stack them
    worldclim_stack <- terra::rast(worldclim_files)
    # calculate the mean or sum
    if (var == "temp") {
      worldclim_mean <- terra::app(worldclim_stack, mean, na.rm = TRUE)
    }
    if (var == "prcp") {
      worldclim_mean <- terra::app(worldclim_stack, sum, na.rm = TRUE)
    }

    # terra::plot(worldclim_mean)

    # save as intermediate data file geotif
    terra::writeRaster(worldclim_mean, str_c("alldata/intermediate/worldclim_", var, ".tif"), overwrite = TRUE)
  }
}
