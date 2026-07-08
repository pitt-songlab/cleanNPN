
#Filter by window function for NPN data
#Written by Brianna Shepherd
#Updated: July 8, 2026

#Load packages
library(readr)
library(tidyverse)

#Import data
ACERflower_2025 <- read_csv("alldata/input/NPN/flower/Acer/2025.csv")

ACERflower_2025 <- ACERflower_2025 %>%
  filter(day_of_year < 182)

start <- NA
end <- 182

#Function writing format

# function_name <- function(parameters){
#   function body 
# }

filter_by_window <- function(parameters){}