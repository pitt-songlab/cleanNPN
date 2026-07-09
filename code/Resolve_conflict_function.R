
#Resolving conflict function for NPN data
#Written by Brianna Shepherd
#Updated July 9, 2026

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

#Consistent

#Experience

#Function writing format-------------------------------------------------------

# function_name <- function(parameters){
#   function body 
# }

resolve_conflict <- function(consistent, experience){}

