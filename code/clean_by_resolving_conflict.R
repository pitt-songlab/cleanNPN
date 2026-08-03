# Resolving conflict function for NPN data
# Written by Brianna Shepherd
# Updated July 29, 2026

clean_by_resolving_conflict <- function(data, calculate_experience_option = c("by_year", "all_years", "other"), calculate_experience_by = NULL) {
  if (calculate_experience_option == "by_year") {
    group_vars <- c("observedby_person_id", "year")
  } else if (calculate_experience_option == "all_years") {
    group_vars <- c("observedby_person_id")
  } else if (calculate_experience_option == "other") {
    group_vars <- c(calculate_experience_by, "observedby_person_id") |>
      unique() |>
      sort()
  }

  data_clean <- data |>
    dplyr::mutate(year = lubridate::year(observation_date)) |>
    # count number of observations for each observer
    dplyr::group_by(!!!syms(group_vars)) |>
    dplyr::mutate(num_of_observations = n()) |>
    dplyr::ungroup() |>
    dplyr::filter(phenophase_status != -1, !is.na(phenophase_status)) |>
    # keep the observation from the observer with the most observations
    dplyr::group_by(individual_id, phenophase_id, observation_date) |>
    dplyr::slice_max(num_of_observations, n = 1, with_ties = T) %>%
    dplyr::ungroup()

  return(data_clean)
}
