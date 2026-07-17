# Filter by trailing yes function for NPN data
# Written by Brianna Shepherd
# Updated July 8, 2026

clean_by_trailing_yes <- function(data, trailing_window_days = 7) {
  data_clean <- data |>
    dplyr::filter(phenophase_status != -1) |>
    # keep one obs per day, yes over no
    dplyr::arrange(individual_id, phenophase_id, observation_date, desc(phenophase_status)) |>
    dplyr::distinct(individual_id, phenophase_id, observation_date, .keep_all = T) |>
    dplyr::group_by(individual_id, phenophase_id) |>
    dplyr::arrange(observation_date, .by_group = TRUE) |>
    # Look forward from tomorrow (.i = obsdate + 1) up to the window limit
    dplyr::mutate(
      trailing_yes_count = slider::slide_index_sum(
        x = (phenophase_status == 1),
        i = observation_date,
        before = -1, # Start counting from tomorrow...
        after = days(trailing_window_days), # ...up until X days from now
        complete = FALSE
      )
    ) |>
    dplyr::ungroup()

  return(data_clean)
}
