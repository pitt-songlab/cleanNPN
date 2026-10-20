#' Count "No" observations in a leading window before each observation
#'
#' For each observation, counts how many "No" (`phenophase_status == 0`)
#' records were made for the same individual and phenophase during a window of
#' days leading up to, but not including, that observation's date. Observations
#' with unknown status are dropped, and duplicate records on the same day are
#' reduced to a single observation determined by `prioritize`.
#'
#' **Sorting and year boundaries.** Observations are ordered by
#' `observation_date`, **not** by `day_of_year`. Because the window is
#' defined using calendar dates, the count can cross over January 1st. For
#' example, with `leading_window_days = 7`, an observation on January 3 will
#' count "No" records from the previous December, and so on across any
#' year boundary. Ordering by `day_of_year` would incorrectly treat late-year
#' observations as occurring after early-year ones, so it is deliberately
#' avoided.
#'
#' **Note:** `leading_no_count` counts the number of "No" *records* in the
#' window, not the number of consecutive "No" days. Days without any
#' observation do not contribute to the count, so gaps in monitoring lower the
#' count rather than interrupting it.
#'
#' @param data A data frame of phenology observations. Must contain the
#'   columns:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase.}
#'     \item{`observation_date`}{Date of the observation. Must be of class
#'       `Date` .}
#'     \item{`phenophase_status`}{Integer status of the phenophase: `1` = yes, `0` = no,
#'       `-1` = unknown.}
#'   }
#' @param leading_window_days A single non-negative number giving the length
#'   of the look back window, in days. The window starts this many days before
#'   each observation date and ends the day before it. Defaults to `7`.
#' @param prioritize Either `"no"` (the default) or `"yes"`. Determines which
#'   record is retained when an individual has more than one record for the same
#'   phenophase on the same date: `"no"` keeps a "No" over a "Yes", and `"yes"`
#'   keeps a "Yes" over a "No".
#'
#' @return A data frame containing the rows of `data` that remain
#'   after removing unknown-status records and same-day duplicates, with all
#'   original columns plus:
#'   \describe{
#'     \item{`leading_no_count`}{The number of "No" records for the same
#'       `individual_id` and `phenophase_id` within the leading window before
#'       each observation date (excluding the date itself).}
#'   }
#'   Rows are sorted by `individual_id`, `phenophase_id`, and then
#'   `observation_date`.
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   observation_date = as.Date(c("2023-12-28", "2023-12-31", "2024-01-02",
#'                                "2024-01-03", "2024-01-03")),
#'   phenophase_status = c(0, 0, 0, 1, 0)
#' )
#'
#' # The Jan 3 duplicate is resolved in favor of "No"; counts for early
#' # January include "No" records from December of the prior year.
#' clean_by_leading_no(obs, leading_window_days = 7, prioritize = "no")
#'
#' # Prefer "Yes" when same-day records conflict
#' clean_by_leading_no(obs, leading_window_days = 7, prioritize = "yes")
#'
#' @seealso [slider::slide_index_sum()]
#' @export
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
