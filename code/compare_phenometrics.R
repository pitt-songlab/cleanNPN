
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
library(dplyr)

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
ACER_flower <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACER,
             aes(x = longitude, y = latitude, color = day_of_year),
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
  labs(title = "Acer Flower", color = "Day of Year")
ACER_flower

#Compare visualization of the two methods for flowers
Flower_comparison <- (ACER_flower_phenometrics + ACER_flower)
Flower_comparison

ggsave(filename = "ACER_2025_FlowerComparison.png", plot = Flower_comparison, width = 10, height = 6, path = "/ix/ysong/projects/phenology-FLS/output/Figures/Acer")

ACER_leaf <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white", linewidth = 0.3) +
  geom_point(data = merged_ACER,
             aes(x = longitude, y = latitude, color = day_of_year_leaf),
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
  labs(title = "Acer Leaf", color = "Day of Year")
ACER_leaf

#Compare visualization of the two methods for flowers
Leaf_comparison <- (ACER_leaf_phenometrics + ACER_leaf)
Leaf_comparison

ggsave(filename = "ACER_2025_LeafComparison.png", plot = Leaf_comparison, width = 10, height = 6, path = "/ix/ysong/projects/phenology-FLS/output/Figures/Acer")

#BRI COME BACK TO THIS AND FIGURE OUT HOW MANY FLS ARE DIFFERENT BETWEEN THE
#TWO DATA FRAMES, I WAS HAVING ISSUES ON 5/29 WITH THIS. SOME TIMES IT SAID
#NO DIFFERENCES OTHER TIMES IT SAID 496 DIFFERENCES
#MAKE SURE IT IS CONSIDERING THERE ARE INDIVIDUAL IDS THAT DATA IS TIED TO WHEN COMPARING

#Compare further with a scatter plot

ggplot(data = NULL, aes(x = merged_ACER$FLS, y = ACER_merged_ind_phen_2025$FLS)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "My FLS", y = "Phenometric FLS", title = "ACER Correlation Plot") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#Significance test
cor.test(merged_ACER$FLS, ACER_merged_ind_phen_2025$FLS, method = "pearson")

# Pearson's product-moment correlation
# 
# data:  merged_ACER$FLS and ACER_merged_ind_phen_2025$FLS
# t = 0.7594, df = 528, p-value = 0.448
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.05228704  0.11786952
# sample estimates:
#        cor 
# 0.03303059 

#identify the mismatches
mismatches <- merged_ACER %>%
  mutate(df2_FLS = ACER_merged_ind_phen_2025$FLS) %>%         # bring in the second column
  filter(FLS != df2_FLS)
mismatches

mismatches <- 
  inner_join(merged_ACER, ACER_merged_ind_phen_2025, by = "individual_id") %>%      # join on matching IDs
  filter(FLS.x != FLS.y)   
mismatches

#Create a joint FLS data frame

mini_ACER <- merged_ACER %>%
  select(individual_id, latitude, longitude, genus, species, common_name, FLS)

mini_phenometrics <- ACER_merged_ind_phen_2025 %>%
  select(individual_id, FLS) %>% 
  rename(phenometric_FLS = FLS)

ACER_FLS <- mini_ACER %>% inner_join(mini_phenometrics, by = "individual_id")

#Compare the FLS
test <- select(merged_ACER$FLS != ACER_merged_ind_phen_2025$FLS)

test <- ACER_FLS %>% filter(FLS == phenometric_FLS)
test

ggplot(ACER_FLS, aes(FLS, phenometric_FLS)) +
  geom_point()
