
#Downloading climate data for FLS
#Adapted by Brianna Shepherd
#Updated 9/3/2026

#Load packages
library(tidyverse)
library(readr)
library(stringr)

#Upload data to extract coordinates
f_flower <- list.files("alldata/input/NPN/flower/", full.names = T, recursive = T)
flower <- read_csv(f_flower) |>
  filter(phenophase_description == "Open flowers")

f_leaf <- list.files("alldata/input/NPN/leaf/", full.names = T, recursive = T)
leaf <- read_csv(f_leaf)

#Count number of unique individuals are in each data frame
length(unique(flower$individual_id)) #18820
length(unique(leaf$individual_id)) #18422

#Group by individual ID and keep latitude and longitude
flower_locations <- flower |> 
  select(site_id, latitude, longitude, state, genus, species, individual_id) |>
  group_by(individual_id) |>
  filter(!duplicated(individual_id))

leaf_locations <- leaf |>
  select(site_id, latitude, longitude, state, genus, species, individual_id) |>
  group_by(individual_id) |>
  filter(!duplicated(individual_id))

#Join data frames, but only add individuals not already found in the flower locations df
locations <- rbind(flower_locations, anti_join(flower_locations, leaf_locations, by = c("individual_id")))

write.csv(locations, "/ix/ysong/projects/phenology-FLS/intermediate/FLS_locations.csv")

#Downloading the weather data
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
    worldclim_url <- str_c("https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_30s_", worldclim_var, ".zip")
    worldclim_dir <- "alldata/input/climate/"
    worldclim_file <- str_c("wc2.1_30s_", worldclim_var)

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

download_terraclim()

#Extracting at my specified coordinate

raster_path <- str_c("alldata/intermediate/worldclim_", clim_var, ".tif")

climate_raster <- terra::rast(raster_path)

coordinate_points <- terra::vect(
  df_coordinates,
  geom = c("decimallongitude", "decimallatitude"),
  crs = "EPSG:4326"
)

extracted_values <- terra::extract(climate_raster, coordinate_points)

df_coordinates_with_climate <- coordinate_points %>%
  as_tibble() %>%
  bind_cols(extracted_values) %>%
  select(-ID)

#TERRACLIM CODE THAT NEEDS ADAPTED TO DOWNLOAD DATA

path_terraclim <- str_c(.path$input, "terraclim/climatology/raw/")

# sites as points
sf_meta <- df_meta %>%
  drop_na(site) %>%
  select(site, lon, lat) %>%
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

# read in raster
ras_tmax <- list.files(path_terraclim, "tmax", full.names = T) %>%
  terra::rast()
ras_tmin <- list.files(path_terraclim, "tmin", full.names = T) %>%
  terra::rast()
ras_ppt <- list.files(path_terraclim, "ppt", full.names = T) %>%
  terra::rast()

# extract climatology data at points
df_terraclim <- sf_meta %>%
  mutate(
    mat = (terra::extract(ras_tmax, sf_meta) %>% select(-ID) %>% rowMeans() + terra::extract(ras_tmin, sf_meta) %>% select(-ID) %>% rowMeans()) / 2,
    tap = terra::extract(ras_ppt, sf_meta) %>% select(-ID) %>% rowSums()
  ) %>%
  as_tibble() %>%
  select(-geometry)

write_rds(df_terraclim, str_c(.path$input, "terraclim/dat_terraclim.rds"))