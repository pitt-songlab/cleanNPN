# Filter by window function for NPN data
# Written by Brianna Shepherd
# Updated: July 9, 2026

clean_by_window <- function(data, min_doy = NULL, max_doy = NULL) {
  if (is.null(min_doy)) {
    min_doy <- -Inf
  }
  if (is.null(max_doy)) {
    max_doy <- Inf
  }
  data_clean <- data %>%
    # #Format the observation date
    # dplyr::mutate(obsdate = lubridate::ymd(observation_date)) |>
    # dplyr::mutate(yr = lubridate::year(observation_date)) |>
    # #Remove observations that are uncertain or not observed (i.e -1 or 0)
    # dplyr::filter(phenophase_status == 1) |>
    # Filter out observations that do not fall in these specific dates
    # Day 213 was used in Taylor et al., 2019 publication
    # "Comparison of large-scale citizen science data and long-term study data for phenology modeling"
    dplyr::filter(
      day_of_year >= min_doy,
      day_of_year <= max_doy
    )

  return(data_clean)
}
