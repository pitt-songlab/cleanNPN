
#Exploratory Data Analysis
#Written by Brianna Shepherd
#Updated; May 18, 2026

#load packages
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(maps)
library(patchwork)

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
maple_flower <- ggplot() +
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
maple_flower

ggsave(filename = "Maple_First_Flower2025.png", plot = maple_flower, path = "/ix/ysong/projects/phenology-FLS/output/Figures/Acer")

#Determine first leaf date for all maples
leaves <- Acer_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1) %>%
  ungroup()

#Visualize
maple_leaf <- ggplot() +
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
maple_leaf

ggsave(filename = "Maple_First_Leaf2025.png", plot = maple_leaf, path = "/ix/ysong/projects/phenology-FLS/output/Figures/Acer")

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

#Using first observation of each phenophase and only keep the first observation if there are multiple for the same individual 

Red_flower25 <- Red_flower25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

Red_leaf25 <- Red_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Summary of ID overlap
cat("IDs in df1:", n_distinct(Red_flower25$individual_id), "\n")#307
cat("IDs in df2:", n_distinct(Red_leaf25$individual_id), "\n")#534
cat("IDs in both:", length(intersect(Red_flower25$individual_id, Red_leaf25$individual_id)), "\n")#272
cat("IDs only in df1:", length(setdiff(Red_flower25$individual_id, Red_leaf25$individual_id)), "\n")#35
cat("IDs only in df2:", length(setdiff(Red_flower25$individual_id, Red_leaf25$individual_id)), "\n")#35

intersect(Red_flower25$individual_id, Red_leaf25$individual_id)

#There are not the same number of individuals in each dataframe, therefore we will do a inner_join to only keep observations in both DF that have leaf
#and flower observed

#rename column headers and select only the necessary coloumns for the join

Red_leaf25 <- Red_leaf25 %>% 
  select(individual_id, phenophase_description, observation_date, day_of_year, phenophase_status, intensity_value) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         observation_date_leaf = observation_date,
         day_of_year_leaf = day_of_year,
         phenophase_status_leaf = phenophase_status,
         intensity_value_leaf = intensity_value)
#join data frames now

merged_ACRU <- inner_join(Red_flower25, Red_leaf25, by = "individual_id")

#Calculate FLS

merged_ACRU$FLS <- (merged_ACRU$day_of_year_leaf - merged_ACRU$day_of_year)


#Visualizations

#Open flowers map

ACRU_Flowers <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRU,
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
  labs(title = "Red Maple Open Flowers")
ACRU_Flowers

#Leaves Map

ACRU_Leaves <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRU,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
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
  labs(title = "Red Maple Leaves")
ACRU_Leaves

#Days between Flower and Leaves

ACRU_FLS <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRU,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of Days",
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
  labs(title = "Red Maple FLS")
ACRU_FLS

#join into one figure

ACRU <- (ACRU_Flowers + ACRU_Leaves + ACRU_FLS)
ACRU


#Try with Peak flower (<75%) and first leaf

#Filter for red maple

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

#Using 50-75% intensity for open flowers and first observation of leaves and only 
#keep the first observation if there are multiple for the same individual 

Red_flower25A <- Red_flower25 %>%
  filter(intensity_value == "50-74%") %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()


Red_leaf25A <- Red_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Summary of ID overlap
cat("IDs in df1:", n_distinct(Red_flower25A$individual_id), "\n")#84
cat("IDs in df2:", n_distinct(Red_leaf25A$individual_id), "\n")#534
cat("IDs in both:", length(intersect(Red_flower25A$individual_id, Red_leaf25A$individual_id)), "\n")#81
cat("IDs only in df1:", length(setdiff(Red_flower25A$individual_id, Red_leaf25A$individual_id)), "\n")#3
cat("IDs only in df2:", length(setdiff(Red_flower25A$individual_id, Red_leaf25A$individual_id)), "\n")#3

intersect(Red_flower25A$individual_id, Red_leaf25A$individual_id)

#81 individuals observed across both dataframes

#rename column headers and select only the necessary coloumns for the join

Red_leaf25A <- Red_leaf25A %>% 
  select(individual_id, phenophase_description, observation_date, day_of_year, phenophase_status, intensity_value) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         observation_date_leaf = observation_date,
         day_of_year_leaf = day_of_year,
         phenophase_status_leaf = phenophase_status,
         intensity_value_leaf = intensity_value)

#join data frames now

merged_ACRUa <- inner_join(Red_flower25A, Red_leaf25A, by = "individual_id")

#Calculate FLS

merged_ACRUa$FLS <- (merged_ACRUa$day_of_year_leaf - merged_ACRUa$day_of_year)

#Visualizations

#Open flowers map

ACRU_FlowersA <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRUa,
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
  labs(title = "Red Maple Open Flowers")
ACRU_FlowersA

#Leaves Map

ACRU_LeavesA <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRUa,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
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
  labs(title = "Red Maple Leaves")
ACRU_LeavesA

#Days between Flower and Leaves

ACRU_FLSa <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRUa,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of Days",
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
  labs(title = "Red Maple FLS")
ACRU_FLSa

#join into one figure

ACRUa <- (ACRU_FlowersA + ACRU_LeavesA + ACRU_FLSa)
ACRUa

#Try with 50 % flower and 50% leaves

#Filter for red maple

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

#Using 50% intensity of each phenophase and only keep the first observation if there are multiple for the same individual 

Red_flower25B <- Red_flower25 %>%
  filter(intensity_value == "50-74%") %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()


Red_leaf25B <- Red_leaf25 %>%
  filter(intensity_value == "50-74%") %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Summary of ID overlap
cat("IDs in df1:", n_distinct(Red_flower25B$individual_id), "\n")#84
cat("IDs in df2:", n_distinct(Red_leaf25B$individual_id), "\n")#269
cat("IDs in both:", length(intersect(Red_flower25B$individual_id, Red_leaf25B$individual_id)), "\n")#45
cat("IDs only in df1:", length(setdiff(Red_flower25B$individual_id, Red_leaf25B$individual_id)), "\n")#39
cat("IDs only in df2:", length(setdiff(Red_flower25B$individual_id, Red_leaf25B$individual_id)), "\n")#39

intersect(Red_flower25B$individual_id, Red_leaf25B$individual_id)

#There are only 45 individuals with observations in both dataframes

#rename column headers and select only the necessary coloumns for the join

Red_leaf25B <- Red_leaf25B %>% 
  select(individual_id, phenophase_description, observation_date, day_of_year, phenophase_status, intensity_value) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         observation_date_leaf = observation_date,
         day_of_year_leaf = day_of_year,
         phenophase_status_leaf = phenophase_status,
         intensity_value_leaf = intensity_value)

#join data frames now

merged_ACRUb <- inner_join(Red_flower25B, Red_leaf25B, by = "individual_id")

#Calculate FLS

merged_ACRUb$FLS <- (merged_ACRUb$day_of_year_leaf - merged_ACRUb$day_of_year)

#Visualizations

#Open flowers map

ACRU_FlowersB <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRUb,
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
  labs(title = "Red Maple Open Flowers")
ACRU_FlowersB

#Leaves Map

ACRU_LeavesB <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRUb,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
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
  labs(title = "Red Maple Leaves")
ACRU_LeavesB

#Days between Flower and Leaves

ACRU_FLSb <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACRUb,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of Days",
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
  labs(title = "Red Maple FLS")
ACRU_FLSb

#join into one figure

ACRUb <- (ACRU_FlowersB + ACRU_LeavesB + ACRU_FLSb)
ACRUb

#Import 2025 Quercus data

Quercus_flower25 <- read_csv("alldata/input/NPN/flower/Quercus/2025.csv")
View(Quercus_flower25)

Quercus_leaf25<- read_csv("alldata/input/NPN/leaf/Quercus/2025.csv")
View(Quercus_leaf25)

#Remove observations that are uncertain or not observed (i.e -1 or 0)

flowers <- Quercus_flower25%>%
  filter(phenophase_status == 1)

leaves <- Quercus_leaf25 %>%
  filter(phenophase_status == 1)

#Load US Map
us_map <- map_data("state")

#Determine first flower date for all observations
flowers <- flowers %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
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
  labs(title = "Quercus Flower")

#Determine first leaf date for all maples
leaves <- Quercus_leaf25 %>%
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
  labs(title = "Quercus leaf")

#Filter for Northern Red oak only to start

Red_flower25 <- Quercus_flower25 %>%
  filter(species == "rubra")

Red_leaf25 <- Quercus_leaf25 %>%
  filter(species == "rubra")

#Remove observations that are uncertain or not observed (i.e -1 or 0)

Red_flower25 <- Red_flower25 %>%
  filter(phenophase_status == 1)

Red_leaf25 <- Red_leaf25%>%
  filter(phenophase_status == 1)

#For flowers, filter out the pollen release information

Red_flower25 <- Red_flower25 %>%
  filter(phenophase_description == "Open flowers")

#Using first observation of each phenophase and only keep the first observation if there are multiple for the same individual 

Red_flower25 <- Red_flower25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

Red_leaf25 <- Red_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Summary of ID overlap
cat("IDs in df1:", n_distinct(Red_flower25$individual_id), "\n")#84
cat("IDs in df2:", n_distinct(Red_leaf25$individual_id), "\n")#210
cat("IDs in both:", length(intersect(Red_flower25$individual_id, Red_leaf25$individual_id)), "\n")#83
cat("IDs only in df1:", length(setdiff(Red_flower25$individual_id, Red_leaf25$individual_id)), "\n")#1
cat("IDs only in df2:", length(setdiff(Red_flower25$individual_id, Red_leaf25$individual_id)), "\n")#1

intersect(Red_flower25$individual_id, Red_leaf25$individual_id)

#Only 83 trees have observations in both dataframes

#Rename column headers and select only the necessary coloumns for the join

Red_leaf25 <- Red_leaf25 %>% 
  select(individual_id, phenophase_description, observation_date, day_of_year, phenophase_status, intensity_value) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         observation_date_leaf = observation_date,
         day_of_year_leaf = day_of_year,
         phenophase_status_leaf = phenophase_status,
         intensity_value_leaf = intensity_value)

#join data frames now

merged_QURU <- inner_join(Red_flower25, Red_leaf25, by = "individual_id")

#Calculate FLS

merged_QURU$FLS <- (merged_QURU$day_of_year_leaf - merged_QURU$day_of_year)

#Visualizations

#Open flowers map

QURU_Flowers <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_QURU,
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
  labs(title = "Northern Red Oak Open Flowers")
QURU_Flowers

#Leaves Map

QURU_Leaves <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_QURU,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
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
  labs(title = "Northern Red Oak Leaves")
QURU_Leaves

#Days between Flower and Leaves

QURU_FLS <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_QURU,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of Days",
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
  labs(title = "Northern Red Oak FLS")
QURU_FLS

#join into one figure

QURU <- (QURU_Flowers + QURU_Leaves + QURU_FLS)
QURU


#Import 2025 Populus data

Populus_flower25 <- read_csv("alldata/input/NPN/flower/Populus/2025.csv")
View(Populus_flower25)

Populus_leaf25 <- read_csv("alldata/input/NPN/leaf/Populus/2025.csv")
View(Populus_leaf25)

#Remove observations that are uncertain or not observed (i.e -1 or 0)

flowers <- Populus_flower25 %>%
  filter(phenophase_status == 1)

leaves <- Populus_leaf25 %>%
  filter(phenophase_status == 1)

#Load US Map
us_map <- map_data("state")

#Determine first flower date for all observations
flowers <- flowers %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
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
  labs(title = "Populus Flower")

#Determine first leaf date for all maples
leaves <- Populus_leaf25 %>%
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
  labs(title = "Populus leaf")

#Filter for Quaking Aspen only to start

Trem_flower25 <- Populus_flower25 %>%
  filter(species == "tremuloides")

Trem_leaf25 <- Populus_leaf25 %>%
  filter(species == "tremuloides")

#Remove observations that are uncertain or not observed (i.e -1 or 0)

Trem_flower25 <- Trem_flower25 %>%
  filter(phenophase_status == 1)

Trem_leaf25 <- Trem_leaf25 %>%
  filter(phenophase_status == 1)

#For flowers, filter out the pollen release information

Trem_flower25 <- Trem_flower25 %>%
  filter(phenophase_description == "Open flowers")

#Using first observation of each phenophase and only keep the first observation if there are multiple for the same individual 

Trem_flower25 <- Trem_flower25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

Trem_leaf25 <- Trem_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Summary of ID overlap
cat("IDs in df1:", n_distinct(Trem_flower25$individual_id), "\n")#28
cat("IDs in df2:", n_distinct(Trem_leaf25$individual_id), "\n")#53
cat("IDs in both:", length(intersect(Trem_flower25$individual_id, Trem_leaf25$individual_id)), "\n")#25
cat("IDs only in df1:", length(setdiff(Trem_flower25$individual_id, Trem_leaf25$individual_id)), "\n")#3
cat("IDs only in df2:", length(setdiff(Trem_flower25$individual_id, Trem_leaf25$individual_id)), "\n")#3

intersect(Trem_flower25$individual_id, Trem_leaf25$individual_id)

#There are only 25 ids that are documented in each dataframe

#Rename column headers and select only the necessary coloumns for the join

Trem_leaf25 <- Trem_leaf25 %>% 
  select(individual_id, phenophase_description, observation_date, day_of_year, phenophase_status, intensity_value) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         observation_date_leaf = observation_date,
         day_of_year_leaf = day_of_year,
         phenophase_status_leaf = phenophase_status,
         intensity_value_leaf = intensity_value)

#join data frames now

merged_POTR <- inner_join(Trem_flower25, Trem_leaf25, by = "individual_id")

#Calculate FLS

merged_POTR$FLS <- (merged_POTR$day_of_year_leaf - merged_POTR$day_of_year)

#Visualizations

#Open flowers map

POTR_Flowers <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_POTR,
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
  labs(title = "Trembling Aspen Open Flowers")
POTR_Flowers

#Leaves Map

POTR_Leaves <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_POTR,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
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
  labs(title = "Trembling Aspen Leaves")
POTR_Leaves

#Days between Flower and Leaves

POTR_FLS <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_POTR,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of Days",
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
  labs(title = "Trembling Aspen FLS")
POTR_FLS

#join into one figure

POTR <- (POTR_Flowers + POTR_Leaves + POTR_FLS)
POTR

#Import 2025 Betula data

Betula_flower25 <- read_csv("alldata/input/NPN/flower/Betula/2025.csv")
View(Betula_flower25)

Betula_leaf25 <- read_csv("alldata/input/NPN/leaf/Betula/2025.csv")
View(Betula_leaf25)

#Remove observations that are uncertain or not observed (i.e -1 or 0)

flowers <- Betula_flower25 %>%
  filter(phenophase_status == 1)

leaves <- Betula_leaf25 %>%
  filter(phenophase_status == 1)

#Load US Map
us_map <- map_data("state")

#Determine first flower date for all observations
flowers <- flowers %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
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
  labs(title = "Betula Flower")

#Determine first leaf date for all maples
leaves <- Betula_leaf25 %>%
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
  labs(title = "Betula leaf")

#Filter for River birch only to start

Riv_flower25 <- Betula_flower25 %>%
  filter(species == "nigra")

Riv_leaf25 <- Betula_leaf25 %>%
  filter(species == "nigra")

#Remove observations that are uncertain or not observed (i.e -1 or 0)

Riv_flower25 <- Riv_flower25 %>%
  filter(phenophase_status == 1)

Riv_leaf25 <- Riv_leaf25 %>%
  filter(phenophase_status == 1)

#For flowers, filter out the pollen release information

Riv_flower25 <- Riv_flower25 %>%
  filter(phenophase_description == "Open flowers")


#Using first observation of each phenophase and only keep the first observation if there are multiple for the same individual 

Riv_flower25 <- Riv_flower25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

Riv_leaf25 <- Riv_leaf25 %>%
  group_by(individual_id) %>%
  slice_min(day_of_year, n = 1, with_ties = FALSE) %>%
  ungroup()

#Summary of ID overlap
cat("IDs in df1:", n_distinct(Riv_flower25$individual_id), "\n")#16
cat("IDs in df2:", n_distinct(Riv_leaf25$individual_id), "\n")#50
cat("IDs in both:", length(intersect(Riv_flower25$individual_id, Riv_leaf25$individual_id)), "\n")#14
cat("IDs only in df1:", length(setdiff(Riv_flower25$individual_id, Riv_leaf25$individual_id)), "\n")#2
cat("IDs only in df2:", length(setdiff(Riv_flower25$individual_id, Riv_leaf25$individual_id)), "\n")#2

intersect(Riv_flower25$individual_id, Riv_leaf25$individual_id)

#There are only 14 individuals with observations in both dataframes

#Rename column headers and select only the necessary coloumns for the join

Riv_leaf25 <- Riv_leaf25 %>% 
  select(individual_id, phenophase_description, observation_date, day_of_year, phenophase_status, intensity_value) %>%
  rename(phenophase_description_leaf =  phenophase_description,
         observation_date_leaf = observation_date,
         day_of_year_leaf = day_of_year,
         phenophase_status_leaf = phenophase_status,
         intensity_value_leaf = intensity_value)

#join data frames now

merged_BENI <- inner_join(Riv_flower25, Riv_leaf25, by = "individual_id")

#Calculate FLS

merged_BENI$FLS <- (merged_BENI$day_of_year_leaf - merged_BENI$day_of_year)

#Visualizations

#Open flowers map

BENI_Flowers <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_BENI,
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
  labs(title = "River birch Open Flowers")
BENI_Flowers

#Leaves Map

BENI_Leaves <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_BENI,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
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
  labs(title = "River Birch Leaves")
BENI_Leaves

#Days between Flower and Leaves

BENI_FLS <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_BENI,
             aes(x = longitude, y = latitude, color = FLS),
             size = 3, alpha = 0.8) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Number of Days",
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
  labs(title = "River Birch FLS")
BENI_FLS

#join into one figure

BENI <- (BENI_Flowers + BENI_Leaves + BENI_FLS)
BENI

#Most individuals have 0 days between FLS, is that due to limited observations or because leaf and flower emerge on the same day?