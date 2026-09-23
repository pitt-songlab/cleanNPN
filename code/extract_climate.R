# YS note: This is not ready. Needs some adaptation.

library(tidyverse)

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

#Extract data points for annual data set

#Set year variable
year <- c(2000:2024)

#Establish path to files
raster_path <- str_c("alldata/intermediate/WorldClim/annual/", "temp_030405_", year, ".tif")

#Create a SpatRaster - a spatially referenced surface divided into three dimensional cells (rows, columns, layers)
climate_raster <- terra::rast(raster_path)

#Change layer names to match year
names(climate_raster) <- str_c(year)

#Import file with locations
library(readr)
FLS_locations <- read_csv("alldata/intermediate/FLS_locations.csv") |>
  select(-`...1`)

#Create SpatVector object
coordinate_points <- terra::vect(
  FLS_locations,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

#Extract the values
extracted_values <- terra::extract(climate_raster, coordinate_points)

#Create data frame with extracted values
df_coordinates_with_annual <- coordinate_points |>
  as_tibble() |>
  bind_cols(extracted_values) |>
  select(-ID)

#Change format of the output dataframe
df_coordinates_annual_long <- df_coordinates_with_annual %>%
  pivot_longer(
    cols = all_of(as.character(year)),
    names_to = "year",
    values_to = "temperature"
  ) %>%
  mutate(year = as.integer(year))

#Save files to output (?) folder
write.csv(df_coordinates_with_annual, "/ix/ysong/projects/phenology-FLS/output/coordinates_with_annualclim.csv")
write.csv(df_coordinates_annual_long, "/ix/ysong/projects/phenology-FLS/output/coordinates_annualclim_long.csv")

################################################################################

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