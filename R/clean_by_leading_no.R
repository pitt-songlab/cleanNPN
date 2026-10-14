
clean_by_leading_no <- function(data, leading_window_days = 7, prioritize = c("no", "yes")) {
  prioritize <- match.arg(prioritize)
  data_clean <- data |>
    dplyr::filter(phenophase_status != -1)

  if (prioritize == "no") {
    data_clean <- data_clean |>
      # keep one obs per day, no over yes
      dplyr::arrange(individual_id, phenophase_id, observation_date, phenophase_status)
  } else if (prioritize == "yes") {
    data_clean <- data_clean |>
      # keep one obs per day, yes over no
      dplyr::arrange(individual_id, phenophase_id, observation_date, dplyr::desc(phenophase_status))
  }

  data_clean <- data_clean |>
    dplyr::distinct(individual_id, phenophase_id, observation_date, .keep_all = TRUE) |>
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
