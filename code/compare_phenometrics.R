
#Exploratory Data Analysis
#Written by Brianna Shepherd
#Updated: May 29, 2026

#load packages
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(maps)
library(patchwork)

#Import individual phenometrics data for ACER 2025 flowers
ACER_ind_phen_2025_flowers <- read_csv("alldata/input/NPN/flower_individual_phenometrics/Acer/2025.csv")
View(ACER_ind_phen_2025_flowers)
#574 individuals 

#Import individual phenometrics for ACER 2025 leaves
ACER_ind_phen_2025_leaves <- read_csv("/ix/ysong/projects/phenology-FLS/input/NPN/leaf_individual_phenometrics/Acer/2025.csv")
#1203 individuals

#Compare how many individuals are in both dataframes
cat("IDs in both:", length(intersect(ACER_ind_phen_2025_flowers$individual_id, ACER_ind_phen_2025_leaves$individual_id)), "\n")
intersect(ACER_ind_phen_2025_flowers$individual_id, ACER_ind_phen_2025_leaves$individual_id)
#530 individuals in both

#rename column headers and select only the necessary coloumns for the join
ACER_ind_phen_2025_leaves <- ACER_ind_phen_2025_leaves %>% 
  select(individual_id, phenophase_description, first_yes_doy, last_yes_doy) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         first_yes_doy_leaf = first_yes_doy,
         last_yes_doy = last_yes_doy)

#join data frames now
ACER_merged_ind_phen_2025 <- inner_join(ACER_ind_phen_2025_flowers, ACER_ind_phen_2025_leaves, by = "individual_id")

#Calculate FLS
ACER_merged_ind_phen_2025$FLS <- (ACER_merged_ind_phen_2025$first_yes_doy_leaf - ACER_merged_ind_phen_2025$first_yes_doy)

#Save merged csv -ONLY NEED TO DO ONCE UNLESS CHANGES ARE MADE ABOVE
write_csv(ACER_merged_ind_phen_2025, "/ix/ysong/projects/phenology-FLS/intermediate/ACER_merged_indphen_2025.csv")

#Load US Map
us_map <- map_data("state")

#Visualize all ACER points with no outliers removed
ACER_flower_phenometrics <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = ACER_merged_ind_phen_2025,
             aes(x = longitude, y = latitude, color = first_yes_doy),
             size = 3, alpha = 0.8) +
  scale_color_gradientn(colors = c("#fff0f3", "#ffccd5", "#ffb3c1", "#ff8fa3", "#ff758f","#ff4d6d","#c9184a","#a4133c","#800f2f", "#590d22" )) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "Acer Flower Individual Phenometrics", color = "Day of Year")
ACER_flower_phenometrics


ggsave(filename = "Maple_First_Flower2025.png", plot = maple_flower, path = "/ix/ysong/projects/phenology-FLS/output/Figures/Acer")


#Visualize
ACER_leaf_phenometrics <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = ACER_merged_ind_phen_2025,
             aes(x = longitude, y = latitude, color = first_yes_doy_leaf),
             size = 3, alpha = 0.8) +
  scale_color_gradientn(colors = c("#D1EFAF", "#BBD997", "#A8CB7C", "#98B76C", "#849F5B", "#6C804C", "#586E3B","#405724", "#2F4814", "#1F320B" )) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "Acer Leaf Individual Phenometrics", color = "Day of Year")
ACER_leaf_phenometrics

#Individual Phenometrics FLS
ACRU_FLS_phenometrics <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = ACER_merged_ind_phen_2025,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  # scale_color_gradientn(colors = c("#005AD9", "#0070FF", "#FFF880", "#FFE135", "#E60026", "#C60012")) +
  scale_color_gradient2(low = "yellow",
                        mid = "white",
                        high = "purple", 
                        midpoint = 0,
                        limits = c(-max(ACER_merged_ind_phen_2025$FLS), max(ACER_merged_ind_phen_2025$FLS))) +
  coord_fixed(1.3, 
              xlim = c(-125, -65), 
              ylim = c(24, 50)) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) + 
  labs(title = "ACER FLS Individual Phenometrics", color = "Number of Days")
ACRU_FLS_phenometrics

#Import 2025 Acer data 
Acer_flower25 <- read_csv("alldata/input/NPN/flower/Acer/2025.csv")
View(Acer_flower25)

Acer_leaf25<- read_csv("alldata/input/NPN/leaf/Acer/2025.csv")
View(Acer_leaf25)

#Filter flower dataframe to remove pollen release

Acer_flower25 <- Acer_flower25 %>%
  filter(phenophase_description == "Open flowers")

#Remove observations that are uncertain or not observed (i.e -1 or 0)
flowers <- Acer_flower25 %>%
  filter(phenophase_status == 1)

leaves <- Acer_leaf25 %>%
  filter(phenophase_status == 1)

#Determine first flower date for all observations
flowers <- flowers %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Determine first leaf date for all maples
leaves <- leaves %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Compare how many individuals are in both dataframes
cat("IDs in both:", length(intersect(flowers$individual_id, leaves$individual_id)), "\n") #530
intersect(flowers$individual_id, leaves$individual_id)

#rename column headers and select only the necessary coloumns for the join
leaves <- leaves %>% 
  select(individual_id, phenophase_description, observation_date, day_of_year, phenophase_status, intensity_value) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         observation_date_leaf = observation_date,
         day_of_year_leaf = day_of_year,
         phenophase_status_leaf = phenophase_status,
         intensity_value_leaf = intensity_value)

#Join data frames now
merged_ACER <- inner_join(flowers, leaves, by = "individual_id")

#Calculate FLS
merged_ACER$FLS <- (merged_ACER$day_of_year_leaf - merged_ACER$day_of_year)

#Save CSV
write_csv(merged_ACER, "/ix/ysong/projects/phenology-FLS/intermediate/ACER_merged_2025.csv")

#Visualize
