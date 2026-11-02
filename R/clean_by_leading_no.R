#' Count "no" observations in a leading window before each observation
#'
#' For each observation, counts how many "no"
#' records were made for the same individual and phenophase during a window of
#' days leading up to, but not including, that observation's date.
#' Conflicting records for the same phenophase on the same date (with both "yes" and "no") are
#' either included or excluded, controlled by `include_conflict`.
#'
#' **Sorting and year boundaries.** Observations are ordered by
#' `observation_date`, **not** by `day_of_year`. Because the window is
#' defined using calendar dates, the count can cross over January 1st. For
#' example, with `leading_window_days = 7`, an observation on January 3 will
#' count "no" records from the previous December, and so on across any
#' year boundary.
#'
#' **Note:** `leading_no_count` counts the number of "no" *records* in the
#' window, not the number of consecutive "no" days. Days without any
#' observation do not contribute to the count.
#'
#' @param data A data frame or tibble of phenology observations. Must contain
#'   the following columns:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase.}
#'     \item{`observation_date`}{Date of the observation. Must be of class
#'       `Date` .}
#'     \item{`phenophase_status`}{Integer status of the phenophase: `1` = yes, `0` = no,
#'       `-1` = uncertain.}
#'   }
#' @param leading_window_days A single positive integer giving the length
#'   of the look back window, in days. The window starts this many days before
#'   each observation date and ends the day before it. Defaults to `7`.
#' @param include_conflict Logical, whether to include conflicting "no" records
#'   when counting leading "no"s. Defaults to `TRUE`.
#'
#' @return A tibble containing all original rows of `data` with a new column
#'   \describe{
#'     \item{`leading_no_count`}{The number of "no" records for the same
#'       `individual_id` and `phenophase_id` within the leading window before
#'       each observation date (excluding the date itself).}
#'   }
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   observation_date = as.Date(c(
#'     "2023-12-28", "2023-12-31", "2024-01-02",
#'     "2024-01-03", "2024-01-03"
#'   )),
#'   phenophase_status = c(0, 0, 0, 1, 0)
#' )
#'
#' # The Jan 3 duplicate is resolved in favor of "no"; counts for early
#' # January include "no" records from December of the prior year.
#' clean_by_leading_no(obs, leading_window_days = 7, include_conflict = TRUE)
#'
#' # Exclude conflicting "no" records
#' clean_by_leading_no(obs, leading_window_days = 7, include_conflict = FALSE)
#'
#' @export
clean_by_leading_no <- function(data, leading_window_days = 7, include_conflict = TRUE) {
  data_clean <- data |>
    dplyr::as_tibble() |>
    dplyr::group_by(individual_id, phenophase_id, observation_date) |>
    dplyr::mutate(is_conflict = any(phenophase_status == 0) & any(phenophase_status == 1)) |>
    dplyr::ungroup() |>
    dplyr::group_by(individual_id, phenophase_id) |>
    dplyr::arrange(observation_date, .by_group = TRUE) |>
    # Look backward from the window limit up to yesterday (.i = obsdate - 1)
    dplyr::mutate(
      leading_no_count = slider::slide_index_sum(
        x = (phenophase_status == 0 & (!is_conflict | include_conflict)), # Count "no" records, optionally including conflicts
        i = observation_date,
        before = lubridate::days(leading_window_days), # Look back X days...
        after = -1, # ...stop at yesterday (-1)
        complete = FALSE
      )
    ) |>
    dplyr::ungroup()

  return(data_clean)
}
