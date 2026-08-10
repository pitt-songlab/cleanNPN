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
