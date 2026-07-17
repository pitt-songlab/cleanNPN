clean_data <- function(data,
                       max_days_to_last_no = NULL,
                       max_days_to_next_yes = NULL,
                       min_doy = NULL, max_doy = NULL) {
  data_clean <- data
  if (!is.null(max_days_to_last_no)) {
    data_clean <- data_clean |>
      clean_by_leading_no()
  }

  if (!is.null(max_days_to_next_yes)) {
    data_clean <- data_clean |>
      clean_by_trailing_yes()
  }

  if (!is.null(max_days_to_last_no)) {
    data_clean <- data_clean |>
      dplyr::filter(
        !is.na(days_to_last_no),
        days_to_last_no <= max_days_to_last_no
      )
  }

  if (!is.null(max_days_to_next_yes)) {
    data_clean <- data_clean |>
      dplyr::filter(
        !is.na(days_to_next_yes),
        days_to_next_yes <= max_days_to_next_yes
      )
  }

  if (!is.null(min_doy) || !is.null(max_doy)) {
    data_clean <- data_clean |>
      clean_by_window(min_doy = min_doy, max_doy = max_doy)
  }
  return(data_clean)
}
