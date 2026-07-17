# Filter by trailing yes function for NPN data
# Written by Brianna Shepherd
# Updated July 8, 2026

clean_by_trailing_yes <- function(data) {
  data_clean <- data |>
    filter(phenophase_status != -1) |>
    # keep one obs per day, yes over no
    arrange(individual_id, phenophase_id, observation_date, desc(phenophase_status)) |>
    distinct(individual_id, phenophase_id, observation_date, .keep_all = T) |>
    # Add information regarding previous observation status and days since that observation
    # (used from Erin Z's code)
    group_by(individual_id, phenophase_id) |>
    mutate(
      next_status = lead(phenophase_status),
      days_to_next_obs = as.numeric(lead(observation_date) - observation_date)
    ) |>
    ungroup() |>
    # Filter for individuals that have at least 2 consecutive yes observations
    filter(phenophase_status == 1 &
      !is.na(next_status) &
      next_status == 1) |>
    rename(days_to_next_yes = days_to_next_obs) |>
    select(-next_status) |>
    full_join(data) |>
    arrange(individual_id, phenophase_id, observation_date)

  return(data_clean)
}
