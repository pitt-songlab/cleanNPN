
clean_by_trailing_yes <- function(data, trailing_window_days = 7, prioritize = "yes") {
  data_clean <- data |>
    dplyr::filter(phenophase_status != -1)

    if (prioritize == "yes") {
      data_clean <- data_clean |>
        # keep one obs per day, yes over no
        dplyr::arrange(individual_id, phenophase_id, observation_date, dplyr::desc(phenophase_status))
    } else if (prioritize == "no") {
      data_clean <- data_clean |>
        # keep one obs per day, no over yes
        dplyr::arrange(individual_id, phenophase_id, observation_date, phenophase_status)
    }

  data_clean <- data_clean |>
    dplyr::distinct(individual_id, phenophase_id, observation_date, .keep_all = TRUE) |>
    dplyr::group_by(individual_id, phenophase_id) |>
    dplyr::arrange(observation_date, .by_group = TRUE) |>
    # Look forward from tomorrow (.i = obsdate + 1) up to the window limit
    dplyr::mutate(
      trailing_yes_count = slider::slide_index_sum(
        x = (phenophase_status == 1),
        i = observation_date,
        before = -1, # Start counting from tomorrow...
        after = lubridate::days(trailing_window_days), # ...up until X days from now
        complete = FALSE
      )
    ) |>
    dplyr::ungroup()

  return(data_clean)
}
