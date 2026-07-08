
#Filter by leading "no" function for NPN data
#Written by Brianna Shepherd
#Updated July 8, 2026

#Load packages
library(readr)
library(tidyverse)

#Import data
ACERflower_2025 <- read_csv("alldata/input/NPN/flower/Acer/2025.csv")

#Arrange individual, phenophase and date (used from Erin Z's code)
test <- ACERflower_2025 %>%
  arrange(individual_id, phenophase_id, observation_date, .keep_all = TRUE)

#Add information regarding previous observation status and days since that observation

test2 <- test %>%
  mutate(indiv_php = paste0(individual_id, "_", phenophase_id),
         previous_status = NA,
         prior_no = NA)

for (i in 2:nrow(test2)) {
  if (test2$indiv_php[i-1] == test2$indiv_php[i]) {
    test2$previous_status[i] <- test2$phenophase_status[i-1]
    test2$prior_no[i] <- as.numeric(test2$obsdate[i] - test2$obsdate[i-1])
  } else {
    test2$previous_status[i] <- NA
    test2$prior_no[i] <- NA
  }
}

#ERROR object 'i' not found

test <- ACERflower_2025 %>%
filter(phenophase_status == 1 &
         !is.na(previous_status) &
         previous_status == 0 &
         prior_no <= 14) %>%

#Function writing format

# function_name <- function(parameters){
#   function body 
# }

filter_by_leading_no <- function(window){}
