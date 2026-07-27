
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

#Count number of observations for each observer
ACERflower_2025<- ACERflower_2025 %>%
  group_by(observedby_person_id) %>%
  mutate(num_of_observations = n()) %>%
  ungroup()

#Determine how many conflicting reports are in the dataframe
Acer_conflicts <- ACERflower_2025 %>%
  group_by(individual_id, day_of_year) %>%
  # filter(any(phenophase_status == 0) & any(phenophase_status== 1)) %>%
  mutate(conflict = case_when(
    any(phenophase_status == 0) & any(phenophase_status == 1) ~ TRUE,
         .default = FALSE )) %>%
  ungroup()
Acer_conflicts

#For individuals with conflicting observations, keep the observation from the observer with
#the most observations in the data frame

non_conflict <- Acer_conflicts %>%
  group_by(individual_id, day_of_year) %>%
  filter(!any(conflict)) %>%
  ungroup()

conflict_resolved <- Acer_conflicts %>%
  group_by(individual_id, day_of_year) %>%
  filter(any(conflict)) %>%
  slice_max(num_of_observations, n = 1, with_ties = FALSE) %>%
  ungroup()

Acer_filtered <- bind_rows(non_conflict, conflict_resolved) %>%
  arrange(individual_id, day_of_year)



#Experience

#Function writing format-------------------------------------------------------


clean_by_resolving_conflict <- function(data, ){
  data_clean <- data |>
    group_by(ObservedBy_Person_ID) |>
    mutate(num_of_observations = n())|>
    dplyr::ungroup()
  
  return(data_clean)
}

