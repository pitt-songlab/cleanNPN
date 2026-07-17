# Filter by leading "no" function for NPN data
# Written by Brianna Shepherd
# Adapted from Erin Zylstra (NPN) code
# Updated July 8, 2026

clean_by_leading_no <- function(data) {
  data_clean <- data |>
    filter(phenophase_status != -1) |>
    # keep one obs per day, no over yes
    arrange(individual_id, phenophase_id, observation_date, phenophase_status) |>
    distinct(individual_id, phenophase_id, observation_date, .keep_all = T) |>
    group_by(individual_id, phenophase_id) |>
    mutate(
      last_status = lag(phenophase_status),
      days_to_last_obs = as.numeric(observation_date - lag(observation_date))
    ) |>
    ungroup() |>
    # Filter for individuals that have at least one leading no observations
    filter(phenophase_status == 1 &
      !is.na(last_status) &
      last_status == 0) |>
    rename(days_to_last_no = days_to_last_obs) |>
    # filter (days_to_last_no <= max_days_to_last_no) |>
    select(-last_status) |>
    full_join(data) |>
    arrange(individual_id, phenophase_id, observation_date)

  return(data_clean)
}
