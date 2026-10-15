#' Count "Yes" observations in a trailing window after each observation
#'
#' For each observation, counts how many "Yes" (`phenophase_status == 1`)
#' records were made for the same individual and phenophase during a window of
#' days following, but not including, that observation's date. Observations
#' with unknown status are dropped, and duplicate records on the same day are
#' reduced to a single observation determined by `prioritize`.
#'
#' **Sorting and year boundaries.** Observations are ordered by
#' `observation_date`, **not** by `day_of_year`. Because the window is
#' defined using calendar dates, the count can cross over January 1st. For
#' example, with `trailing_window_days = 7`, an observation on December 29 will
#' count "Yes" records from the following January, and so on across any
#' year boundary. Ordering by `day_of_year` would incorrectly treat early-year
#' observations as preceding late-year ones, so it is deliberately avoided.
#'
#' **Note:** `trailing_yes_count` counts the number of "Yes" *records* in the
#' window, not the number of consecutive "Yes" days. Days without any
#' observation do not contribute to the count, so gaps in monitoring lower the
#' count rather than interrupting it.
#'
#' This function is the forward-looking counterpart to
#' [clean_by_leading_no()], which counts "No" records in the window *before*
#' each observation.
#'
#' @param data A data frame of phenology observations. Must contain the
#'   columns:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase.}
#'     \item{`observation_date`}{Date of the observation (class `Date`).}
#'     \item{`phenophase_status`}{Integer status: `1` = yes, `0` = no,
#'       `-1` = unknown.}
#'   }
#' @param trailing_window_days A single non-negative number giving the length
#'   of the forward-looking window, in days. The window starts the day after
#'   each observation date and ends this many days after it. Defaults to `7`.
#' @param prioritize Either `"yes"` (the default) or `"no"`. Determines which
#'   record is retained when an individual has more than one record for the same
#'   phenophase on the same date: `"yes"` keeps a "Yes" over a "No", and `"no"`
#'   keeps a "No" over a "Yes".
#'
#' @return A data frame containing the rows of `data` that remain
#'   after removing unknown-status records and same-day duplicates, with all
#'   original columns plus:
#'   \describe{
#'     \item{`trailing_yes_count`}{The number of "Yes" records for the same
#'       `individual_id` and `phenophase_id` within the trailing window after
#'       each observation date (excluding the date itself).}
#'   }
#'   Rows are sorted by `individual_id`, `phenophase_id`, and then
#'   `observation_date`.
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   observation_date = as.Date(c("2023-12-27", "2023-12-29", "2024-01-02",
#'                                "2024-01-03", "2024-01-03")),
#'   phenophase_status = c(0, 0, 1, 1, 0)
#' )
#'
#' # The Jan 3 duplicate is resolved in favor of "Yes"; counts for late
#' # December include "Yes" records from January of the following year.
#' clean_by_trailing_yes(obs, trailing_window_days = 7, prioritize = "yes")
#'
#' # Prefer "No" when same-day records conflict
#' clean_by_trailing_yes(obs, trailing_window_days = 7, prioritize = "no")
#'
#' @seealso [clean_by_leading_no()], [slider::slide_index_sum()]
#' @export
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
