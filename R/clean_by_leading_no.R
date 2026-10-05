#' Filter by leading "no" observations for NPN phenology data
#'
#' @description
#' Cleans National Phenology Network (NPN) status-and-intensity data and
#' computes, for each observation, a count of "no" (\code{phenophase_status == 0})
#' records reported within a trailing window of days immediately preceding
#' (but not including) that observation date. This "leading no" count is
#' commonly used to help confirm genuine phenophase onset events by requiring
#' a run of negative observations in the days just before a reported "yes".
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Removes records with \code{phenophase_status == -1} (unknown / not
#'     observed).
#'   \item Collapses duplicate observations for the same individual,
#'     phenophase, and date, preferring a "no" (0) over a "yes" (1) when both
#'     are reported on the same day.
#'   \item Groups records by \code{individual_id} and \code{phenophase_id} and
#'     arranges them chronologically within each group.
#'   \item For each observation, sums the "no" records falling within
#'     \code{leading_window_days} days before that observation, stopping the
#'     day before the observation (i.e., the window does not include the
#'     observation date itself).
#' }
#'
#' @param data A data frame (or tibble) of NPN status-and-intensity records.
#'   Must contain the columns \code{individual_id}, \code{phenophase_id},
#'   \code{observation_date} (class \code{Date}), and \code{phenophase_status}
#'   (coded \code{-1}, \code{0}, \code{1}).
#' @param leading_window_days Numeric. Size, in days, of the trailing window
#'   used to count prior "no" observations. Defaults to \code{7}.
#'
#' @return A data frame with the same structure as \code{data} (minus records
#'   with \code{phenophase_status == -1} and duplicate same-day observations),
#'   plus one additional column:
#'   \describe{
#'     \item{leading_no_count}{Integer. Number of "no"
#'       (\code{phenophase_status == 0}) observations recorded for that
#'       \code{individual_id}/\code{phenophase_id} combination within the
#'       \code{leading_window_days} days strictly before the current
#'       \code{observation_date}.}
#'   }
#'
#' @section Author:
#' Written by Brianna Shepherd. Adapted from code by Erin Zylstra (USA
#' National Phenology Network).
#'
#' @examples
#' \dontrun{
#' clean_data <- clean_by_leading_no(npn_data, leading_window_days = 7)
#' }
#'
#' @export
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
