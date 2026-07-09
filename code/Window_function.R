
#Filter by window function for NPN data
#Written by Brianna Shepherd
#Updated: July 9, 2026

#Load packages
library(readr)
library(tidyverse)

#Import data - FLOWERS
ACERflower_2025 <- read_csv("alldata/input/NPN/flower/Acer/2025.csv")

#Remove "pollen release" from dataframe
ACERflower_2025 <- ACERflower_2025 %>%
  filter(phenophase_description == "Open flowers")

#Format the observation date
ACERflower_2025 <- ACERflower_2025 %>%
  mutate(obsdate = ymd(observation_date)) %>%
  mutate(yr = year(observation_date))

#Remove observations that are uncertain or not observed (i.e -1 or 0)
ACERflower_2025 <- ACERflower_2025 %>%
  filter(phenophase_status == 1)

#Filter out observations that do not fall in these specific dates
#Day 213 was used in Taylor et al., 2019 publication
#"Comparison of large-scale citizen science data and long-term study data for phenology modeling"
ACERflower_2025 <- ACERflower_2025 %>%
  filter(day_of_year <= 213)

#Import data - leaves

ACERleaves_2025 <- read_csv("alldata/input/NPN/leaf/Acer/2025.csv")

#Format the observation date
ACERleaves_2025 <- ACERleaves_2025 %>%
  mutate(obsdate = ymd(observation_date)) %>%
  mutate(yr = year(observation_date))

#Remove observations that are uncertain or not observed (i.e -1 or 0)
ACERleaves_2025 <- ACERleaves_2025 %>%
  filter(phenophase_status == 1)

#Filter out observations that do not fall in these specific dates
#just used the same data (August 1)
ACERleaves_2025 <- ACERleaves_2025 %>%
  filter(day_of_year <= 213)


#Function writing format -------------------------------------------------------

# function_name <- function(parameters){
#   function body 
# }

filter_by_window <- function(start, end){}