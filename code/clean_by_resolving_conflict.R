# Resolving conflict function for NPN data
# Written by Brianna Shepherd & Yiluan Song
# Updated August 6, 2026

clean_by_resolving_conflict <- function(data, calculate_observation_option = c("by_year", "all_years", "other"), calculate_observation_by = NULL) {
  #set which columns to group observation data by
  if (calculate_observation_option == "by_year") {
    group_vars <- c("observedby_person_id", "year")
    #checks if option is "all_years", if true, only group by observer
    } else if (calculate_observation_option == "all_years") {
    group_vars <- c("observedby_person_id")
    #checks if option is "other", combines whatever specified columns into one vector
    } else if (calculate_observation_option == "other") {
    group_vars <- c(calculate_observation_by, "observedby_person_id") |>
    #remove duplicate entries  
      unique() |>
      sort()
  }

  data_clean <- data |>
    #change observation_date to proper formatting
    dplyr::mutate(year = lubridate::year(observation_date)) |>
    # count number of observations for each observer
    dplyr::group_by(!!!syms(group_vars)) |>
    dplyr::mutate(num_of_observations = n()) |>
    dplyr::ungroup() |>
    #remove any uncertain observations from the dataframe
    dplyr::filter(phenophase_status != -1, !is.na(phenophase_status)) |>
    # keep the observation from the observer with the most observations
    dplyr::group_by(individual_id, phenophase_id, observation_date) |>
    dplyr::slice_max(num_of_observations, n = 1, with_ties = T) %>%
    dplyr::ungroup()

  return(data_clean)
}

#######################################################################

#Brianna's original version of the function 

clean_by_resolving_conflict <- function(data){
  data_user_observation <- data |>
    #Count number of observations for each observer
    dplyr::group_by(observedby_person_id) |>
    dplyr::mutate(num_of_observations = n()) |>
    dplyr::ungroup()
  
  #Determine how many conflicting reports are in the dataframe
  data_conflicts <- data_user_observation |>
    dplyr::group_by(individual_id, day_of_year) |>
    dplyr::mutate(conflict = dplyr::case_when(
      any(phenophase_status == 0) & any(phenophase_status == 1) ~ TRUE,
      .default = FALSE )) |>
    dplyr::ungroup()
  
  #For individuals with conflicting observations, keep the observation from the observer with
  #the most observations in the data frame
  
  non_conflict <- data_conflicts |>
    dplyr::group_by(individual_id, day_of_year) |>
    dplyr::filter(!any(conflict)) |>
    dplyr::ungroup()
  
  conflict_resolved <- data_conflicts |>
    dplyr::group_by(individual_id, day_of_year) |>
    dplyr::filter(any(conflict)) |>
    dplyr::slice_max(num_of_observations, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  
  data_filtered <- bind_rows(non_conflict, conflict_resolved) |>
    dplyr::arrange(individual_id, day_of_year)
  
  return(data_filtered)
}