library(tidyverse)

data_path <- "alldata/input/NPN/"

path_leaf <- paste0(data_path, "leaf/")
path_flower <- paste0(data_path, "flower/")

species_list <- rnpn::npn_species()
genus_list <- c(
  "Acer",
  "Quercus",
  "Betula",
  "Populus"
)

year_list <- 1900:(Sys.Date() |> lubridate::year())

###
for (genusoi in genus_list) {
  spid <- species_list |>
    filter(genus == genusoi) |>
    select(species_id) |>
    unique()

  for (year in year_list) {
    npn_leaf_df <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 3
    )

    if (nrow(npn_leaf_df) > 0) {
      file <- str_c(data_path, "leaf", "/", genusoi, "/", year, ".csv")
      dir.create(dirname(file), recursive = T, showWarnings = F)

      write.csv(
        x = npn_leaf_df,
        file = file,
        row.names = FALSE,
        quote = TRUE
      )

      print(file)
    }
  }

  for (year in year_list) {
    npn_flower_df_1 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 7
    )
    npn_flower_df_2 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 8
    )
    npn_flower_df <- rbind(npn_flower_df_1, npn_flower_df_2)

    if (nrow(npn_flower_df) > 0) {
      file <- str_c(data_path, "flower", "/", genusoi, "/", year, ".csv")
      dir.create(dirname(file), recursive = T, showWarnings = F)

      write.csv(
        x = npn_flower_df,
        file = file,
        row.names = FALSE,
        quote = TRUE
      )

      print(file)
    }
  }
}

#Exploratory Data Analysis

#load packages
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(maps)

#Import 2025 Acer data 
Acer_flower25 <- read_csv("alldata/input/NPN/flower/Acer/2025.csv")
View(Acer_flower25)

Acer_leaf25<- read_csv("alldata/input/NPN/leaf/Acer/2025.csv")
View(Acer_leaf25)

#Remove observations that are uncertain or not observed (i.e -1 or 0)

flowers <- Acer_flower25 %>%
  filter(phenophase_status == 1)

leaves <- Acer_leaf25 %>%
  filter(phenophase_status == 1)

#Load US Map
us_map <- map_data("state")

#Determine first flower date for all observations
flowers <- flowers %>%
group_by(individual_id) %>%
  slice_min(day_of_year, n = 1) %>%
  ungroup()

#Visualize
ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = flowers,
             aes(x = longitude, y = latitude, color = day_of_year),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Julian Date",
    direction = -1
  ) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "Acer Flower")

#Determine first leaf date for all maples
leaves <- Acer_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1) %>%
  ungroup()

#Visualize
ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = leaves,
             aes(x = longitude, y = latitude, color = day_of_year),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Julian Date",
    direction = -1
  ) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "Acer leaf")

#Filter for red maple only to start

Red_flower25 <- Acer_flower25 %>%
  filter(species == "rubrum")

Red_leaf25 <- Acer_leaf25 %>%
  filter(species == "rubrum")

#Remove observations that are uncertain or not observed (i.e -1 or 0)

Red_flower25 <- Red_flower25 %>%
  filter(phenophase_status == 1)

Red_leaf25 <- Red_leaf25%>%
  filter(phenophase_status == 1)

#for flowers, filter out the pollen release information

Red_flower25 <- Red_flower25 %>%
  filter(phenophase_description == "Open flowers")

#Using first observation of each phenophase

test_flower <- Red_flower25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1) %>%
  ungroup()

test_leaf <- Red_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1) %>%
  ungroup()

#Summary of ID overlap
cat("IDs in df1:", n_distinct(test_flower$individual_id), "\n")#307
cat("IDs in df2:", n_distinct(test_leaf$individual_id), "\n")#534
cat("IDs in both:", length(intersect(test_flower$individual_id, test_leaf$individual_id)), "\n")#272
cat("IDs only in df1:", length(setdiff(test_flower$individual_id, test_leaf$individual_id)), "\n")#35
cat("IDs only in df2:", length(setdiff(test_flower$individual_id, test_leaf$individual_id)), "\n")#35

intersect(test_flower$individual_id, test_leaf$individual_id)

#There are not the same number of individuals in each dataframe, therefore we will need to account for the differences when we calculate FLS

#rename column headers

test_leaf <- test_leaf %>% rename(phenophase_description_leaf =  phenophase_description,
                                  observation_date_leaf = observation_date,
                                  day_of_year_leaf = day_of_year,
                                  phenophase_status_leaf = phenophase_status,
                                  intensity_value_leaf = intensity_value)
#join data frames now

join <- test_flower %>% 
  left_join(test_leaf %>% select(individual_id, phenophase_description_leaf, observation_date_leaf, day_of_year_leaf, phenophase_status_leaf, intensity_value_leaf))

#getting this warning 
# #Warning message:
# In left_join(., test_leaf %>% select(individual_id, phenophase_description_leaf,  :
#                                        Detected an unexpected many-to-many relationship between `x` and `y`.
#                                      ℹ Row 4 of `x` matches multiple rows in `y`.
#                                      ℹ Row 34 of `y` matches multiple rows in `x`.
#                                      ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.

#Calculate FLS

join$FLS <- (join$day_of_year_leaf - join$day_of_year)



# Load US map data
us_map <- map_data("state")

ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = join,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Days between FLS",
    direction = -1
  ) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "Red Maple")

#make a map with just the flowering dates
ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = join,
             aes(x = longitude, y = latitude, color = day_of_year),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "First Flowering Date",
    direction = -1
  ) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "Red Maple")

#First leaf out date
ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = test_leaf,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "First Leaf out Date",
    direction = -1
  ) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "Red Maple")
