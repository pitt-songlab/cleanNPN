# Filter by leading "no" function for NPN data
# Written by Brianna Shepherd
# Adapted from Erin Zylstra (NPN) code
# Updated July 8, 2026

clean_by_leading_no <- function(data, leading_window_days = 7) {
  data_clean <- data |>
    dplyr::filter(phenophase_status != -1) |>
    # keep one obs per day, no over yes
    dplyr::arrange(individual_id, phenophase_id, observation_date, phenophase_status) |>
    dplyr::distinct(individual_id, phenophase_id, observation_date, .keep_all = T) |>
    dplyr::group_by(individual_id, phenophase_id) |>
    dplyr::arrange(observation_date, .by_group = TRUE) |>
    # Look backward from the window limit up to yesterday (.i = obsdate - 1)
    dplyr::mutate(
      leading_no_count = slider::slide_index_sum(
        x = (phenophase_status == 0),
        i = observation_date,
        before = lubridate::days(leading_window_days), # Look back X days...
        after = -1, # ...stop at yesterday (-1)
        complete = FALSE
      )
    ) |>
    dplyr::ungroup()

  return(data_clean)
}
