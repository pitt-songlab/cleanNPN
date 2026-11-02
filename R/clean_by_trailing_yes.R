#' Count "yes" observations in a trailing window after each observation
#'
#' For each observation, counts how many "yes" records were made for the same
#' individual and phenophase during a window of days following, but not
#' including, that observation's date. Conflicting records (records for the
#' same individual and phenophase on the same date that include both a "yes"
#' and a "no") are either included or excluded, as controlled by
#' `include_conflict`.
#'
#' @details
#' **Important notes:**
#' \itemize{
#'   \item Observations are ordered by
#'     `observation_date`, **not** by `day_of_year`. Because the window is
#'     defined using calendar dates, the count can cross over January 1st. For
#'     example, with `trailing_window_days = 7`, an observation on December 29 will
#'     count "yes" records from the following January, and so on across any
#'     year boundary.
#'   \item `trailing_yes_count` counts the number of "yes"
#'     **records** in the window, not the number of consecutive "yes" days. Days
#'     without any observation do not contribute to the count.
#'   \item This function is the forward-looking counterpart to
#'     [clean_by_leading_no()], which counts "no" records in the window **before**
#'     each observation.
#' }
#'
#' @param data A data frame or tibble of phenophase observations. Must contain
#'   the following columns:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase.}
#'     \item{`observation_date`}{Date of the observation. Must be of class
#'       `Date`.}
#'     \item{`phenophase_status`}{Integer status of the phenophase: `1` = yes,
#'       `0` = no, `-1` = uncertain.}
#'   }
#' @param trailing_window_days A single postive number giving the length
#'   of the forward-looking window, in days. The window starts the day after
#'   each observation date and ends this many days after it. Defaults to `7`.
#' @param include_conflict A single logical value specifying whether to
#'   include conflicting "yes" records when counting trailing "yes" records.
#'   Defaults to `TRUE`.
#'
#' @return A tibble containing all rows of `data`, sorted by `individual_id`,
#'   `phenophase_id`, and `observation_date`, with all original columns plus
#'   two new columns:
#'   \describe{
#'     \item{`trailing_yes_count`}{The number of "yes" records for the same
#'       `individual_id` and `phenophase_id` within the trailing window after
#'       each observation date (excluding the date itself).}
#'     \item{`is_conflict`}{Logical. `TRUE` if the record's individual,
#'       phenophase, and date have both a "yes" and a "no" record.}
#'   }
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   observation_date = as.Date(c(
#'     "2023-12-27", "2023-12-29", "2024-01-02",
#'     "2024-01-03", "2024-01-03"
#'   )),
#'   phenophase_status = c(0, 0, 1, 1, 0)
#' )
#'
#' # The two records on Jan 3 conflict (one "yes" and one "no"). Counts for
#' # late December include "yes" records from January of the following year.
#' clean_by_trailing_yes(obs, trailing_window_days = 7, include_conflict = TRUE)
#'
#' # Exclude conflicting "yes" records
#' clean_by_trailing_yes(obs, trailing_window_days = 7, include_conflict = FALSE)
#'
#' @export
clean_by_trailing_yes <- function(data, trailing_window_days = 7, include_conflict = TRUE) {
  data_clean <- data |>
    dplyr::as_tibble() |>
    dplyr::group_by(individual_id, phenophase_id, observation_date) |>
    dplyr::mutate(is_conflict = any(phenophase_status == 0) & any(phenophase_status == 1)) |>
    dplyr::ungroup() |>
    dplyr::group_by(individual_id, phenophase_id) |>
    dplyr::arrange(observation_date, .by_group = TRUE) |>
    # Look forward from tomorrow (.i = obsdate + 1) up to the window limit
    dplyr::mutate(
      trailing_yes_count = slider::slide_index_sum(
        x = (phenophase_status == 1 & (!is_conflict | include_conflict)), # Count "yes" records, optionally including conflicts
        i = observation_date,
        before = -1, # Start counting from tomorrow...
        after = lubridate::days(trailing_window_days), # ...up until X days from now
        complete = FALSE
      )
    ) |>
    dplyr::ungroup()

  return(data_clean)
}
