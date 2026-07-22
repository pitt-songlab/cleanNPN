
#Resolving conflict function for NPN data
#Written by Brianna Shepherd
#Updated July 22, 2026

#Load packages
library(readr)
library(tidyverse)

#Import data - FLOWERS
ACERflower_2025 <- read_csv("alldata/input/NPN/flower/Acer/2025.csv")

#Remove "pollen release" from dataframe
ACERflower_2025 <- ACERflower_2025 %>%
  filter(phenophase_description == "Open flowers")

# #Format the observation date
# ACERflower_2025 <- ACERflower_2025 %>%
#   mutate(obsdate = ymd(observation_date)) %>%
#   mutate(yr = year(observation_date))

conflicts <- ACERflower_2025 %>%
  group_by(individual_id, day_of_year) %>%
  filter(any(phenophase_status ==0) & any(phenophase_status== 1)) %>%
  ungroup() %>%
  arrange(individual_id, day_of_year)

conflicts

#Consistent

#Experience

#Function writing format-------------------------------------------------------


clean_by_resolving_conflict <- function(data, ){
  data_clean <- data |>
    
    |>
    dplyr::ungroup()
  
  return(data_clean)
}

