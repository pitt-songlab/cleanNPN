clean_data <- function(data,
                       min_leading_no = NULL,
                       leading_window_days = NULL,
                       min_trailing_yes = NULL,
                       trailing_window_days = NULL,
                       min_doy = NULL, max_doy = NULL) {
  data_clean <- data
  if (!is.null(leading_window_days) && !is.null(min_leading_no)) {
    data_clean <- data_clean |>
      clean_by_leading_no(leading_window_days = leading_window_days)
  }

  if (!is.null(trailing_window_days) && !is.null(min_trailing_yes)) {
    data_clean <- data_clean |>
      clean_by_trailing_yes(trailing_window_days = trailing_window_days)
  }

  if (!is.null(leading_window_days) && !is.null(min_leading_no)) {
    data_clean <- data_clean |>
      dplyr::filter(
        leading_no_count >= min_leading_no
      )
  }

  if (!is.null(trailing_window_days) && !is.null(min_trailing_yes)) {
    data_clean <- data_clean |>
      dplyr::filter(
        trailing_yes_count >= min_trailing_yes
      )
  }

  if (!is.null(min_doy) || !is.null(max_doy)) {
    data_clean <- data_clean |>
      clean_by_window(min_doy = min_doy, max_doy = max_doy)
  }
  return(data_clean)
}
