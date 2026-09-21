
#written by Yiluan Song & Brianna Shepherd
#Updated September 21, 2026

library(tidyverse)
library(terra)

process_worldclim_climatology <- function(v_var = c("temp", "prcp"), worldclim_dir = "alldata/input/WorldClim/climatology/", process_dir = "alldata/intermediate/WorldClim/climatology/") {
  for (var in v_var) {
    if (var == "temp") {
      worldclim_var <- "tavg"
    }
    if (var == "prcp") {
      worldclim_var <- "prec"
    }

    # read WorldClim data
    worldclim_file <- str_c("wc2.1_30s_", worldclim_var)
    worldclim_files <- list.files(
      str_c(worldclim_dir, worldclim_file),
      pattern = ".tif$",
      full.names = TRUE
    )
    # read all rasters and stack them
    worldclim_stack <- terra::rast(worldclim_files)
    # calculate the mean or sum
    # need large RAM for this step
    if (var == "temp") {
      worldclim_process <- mean(worldclim_stack, na.rm = TRUE)
    }
    if (var == "prcp") {
      worldclim_process <- sum(worldclim_stack, na.rm = TRUE)
    }

    # terra::plot(worldclim_process)

    # save as intermediate data file geotif
    process_file <- str_c(process_dir, "worldclim_", var, ".tif")
    terra::writeRaster(worldclim_process, process_file, overwrite = TRUE)
  }
}

#Process annual data - just temperature

process_worldclim_annual <- function(v_month = c("03", "04", "05"), worldclim_dir = "alldata/input/WorldClim/annual/", process_dir = "alldata/intermediate/WorldClim/annual/") {
  for (year in 2000:2024) {
    
    ls_worldclim_file_full <- list()
    for (worldclim_var in c("tmax", "tmin")) {
      worldclim_file <- str_c("wc2.1_cruts4.09_10m_", worldclim_var, "_", year, "-", v_month, ".tif" )
      ls_worldclim_file_full[[worldclim_var]] <- file.path(worldclim_dir, worldclim_var,  worldclim_file)
    }
    worldclim_file_full <- unlist(ls_worldclim_file_full, use.names = F)
    
    # worldclim_file_tmax <- str_c("wc2.1_cruts4.09_10m_", "tmax", "_", year, "-", v_month, ".tif" ) 
    # worldclim_file_full_tmax <- file.path(worldclim_dir, "tmax",  worldclim_file_tmax)
    # 
    # worldclim_file_tmin <- str_c("wc2.1_cruts4.09_10m_", "tmin", "_", year, "-", v_month, ".tif" ) 
    # worldclim_file_full_tmin <- file.path(worldclim_dir, "tmin",  worldclim_file_tmin)
    # 
    # worldclim_file_full <- c(worldclim_file_full_tmax, worldclim_file_full_tmin)
    
      worldclim_stack <- terra::rast(worldclim_file_full)
      
      # calculate the mean
      # need large RAM for this step (I used 50GB)
      worldclim_process <- terra::mean(worldclim_stack, na.rm = TRUE)
      
      # save as intermediate data file geotiff
      process_file <- str_c(process_dir, "temp_", paste(v_month, collapse = ""),"_", year, ".tif")
      terra::writeRaster(worldclim_process, process_file, overwrite = TRUE)
      
    }
  }
process_worldclim_annual()

# for (year in xxx) {
#   # find relevant file paths for tmin tmax
#   # stack all rasters
#   # calculate mean
#   # save mean raster as geotiff
# }