clean_data <- function(data,
                       calculate_experience_option = NULL,
                       calculate_experience_by = NULL,
                       min_leading_no = NULL,
                       leading_window_days = NULL,
                       min_trailing_yes = NULL,
                       trailing_window_days = NULL,
                       min_doy = NULL, max_doy = NULL) {
  data_clean <- data

  if (!is.null(calculate_experience_option)) {
    message("Cleaning by resolving conflict")
    data_clean <- data_clean |>
      clean_by_resolving_conflict(
        calculate_experience_option = calculate_experience_option,
        calculate_experience_by = calculate_experience_by
      )
  }

  if (!is.null(leading_window_days) && !is.null(min_leading_no)) {
    message("Cleaning by leading no")
    data_clean <- data_clean |>
      clean_by_leading_no(leading_window_days = leading_window_days)
  }

  if (!is.null(trailing_window_days) && !is.null(min_trailing_yes)) {
    message("Cleaning by trailing yes")
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
    message("Cleaning by window")
    data_clean <- data_clean |>
      clean_by_window(min_doy = min_doy, max_doy = max_doy)
  }
  return(data_clean)
}
