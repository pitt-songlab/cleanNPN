
#Filter by leading "no" function for NPN data
#Written by Brianna Shepherd
#Adapted from Erin Zylstra (NPN) code
#Updated July 8, 2026

#Load packages
library(readr)
library(tidyverse)

#Import data
ACERflower_2025 <- read_csv("alldata/input/NPN/flower/Acer/2025.csv")

#Remove "pollen release" from dataframe
ACERflower_2025 <- ACERflower_2025 %>%
  filter(phenophase_description == "Open flowers")

#Format the observation date
ACERflower_2025 <- ACERflower_2025 %>%
  mutate(obsdate = ymd(observation_date)) %>%
  mutate(yr = year(observation_date))

#Arrange individual, phenophase and date (used from Erin Z's code)
ACERflower_2025 <- ACERflower_2025 %>%
  arrange(individual_id, phenophase_id, obsdate, .keep_all = TRUE)

#Add information regarding previous observation status and days since that observation
#(used from Erin Z's code)
ACERflower_2025 <- ACERflower_2025 %>%
  mutate(indiv_php = paste0(individual_id, "_", phenophase_id),
         previous_status = NA,
         prior_no = NA)

for (i in 2:nrow(ACERflower_2025)) {
  if (ACERflower_2025$indiv_php[i-1] == ACERflower_2025$indiv_php[i]) {
    ACERflower_2025$previous_status[i] <- ACERflower_2025$phenophase_status[i-1]
    ACERflower_2025$prior_no[i] <- as.numeric(ACERflower_2025$obsdate[i] - ACERflower_2025$obsdate[i-1])
  } else {
    ACERflower_2025$previous_status[i] <- NA
    ACERflower_2025$prior_no[i] <- NA
  }
}

#Filter dataframe (used from Erin Z's code)
clean_ACERflower25 <- ACERflower_2025 %>%
filter(phenophase_status == 1 &
         !is.na(previous_status) &
         previous_status == 0 &
         prior_no <= 14) %>%
  arrange(individual_id, phenophase_id, obsdate) %>%
  distinct(individual_id, phenophase_id, yr, .keep_all = TRUE)

#Function writing format ------------------------------------------------------
#NOT COMPLETED - FOR THE NEXT STEP

# function_name <- function(parameters){
#   function body 
# }

filter_by_leading_no <- function(window){}
