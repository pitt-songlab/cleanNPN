#' Clean phenophase observations using a configurable sequence of filters
#'
#' A wrapper that applies up to four cleaning steps to phenophase observation
#' data. Each step is optional and runs only when its arguments are supplied,
#' so you can combine them as needed. Rows that fail the thresholds you set
#' are removed from the output.
#'
#' @details
#' Steps run in the following order, and a step is skipped unless all of its
#' required arguments are non-`NULL`:
#' \enumerate{
#'   \item **Resolve conflicts** (requires `calculate_observation_option`):
#'     where several observers report on the same individual, phenophase, and
#'     date, keeps the record(s) from the observer with the most observations.
#'     Calls [clean_by_resolving_conflict()].
#'   \item **Leading "no" count** (requires `leading_window_days` and
#'     `min_leading_no`): adds `leading_no_count`, the number of "no"
#'     observations in the window preceding each observation. Calls
#'     [clean_by_leading_no()].
#'   \item **Trailing "yes" count** (requires `trailing_window_days` and
#'     `min_trailing_yes`): adds `trailing_yes_count`, the number of "yes"
#'     observations in the window following each observation. Calls
#'     [clean_by_trailing_yes()].
#'   \item **Threshold filtering**: keeps only rows where
#'     `leading_no_count >= min_leading_no` and/or
#'     `trailing_yes_count >= min_trailing_yes`, for whichever steps were
#'     requested.
#'   \item **Day-of-year window** (requires `min_doy` and/or `max_doy`):
#'     restricts observations to the given day-of-year range. Calls
#'     [clean_by_window()].
#' }
#'
#' **Important notes:**
#' \itemize{
#'   \item Both the leading and trailing counts are calculated *before* either
#'     threshold filter is applied, so filtering on one count does not change
#'     the other. The counts are also computed after conflict resolution, so
#'     they reflect the data left by step 1.
#'   \item The leading and trailing steps each require *both* of their
#'     arguments. If only one is supplied (e.g., `min_trailing_yes` without
#'     `trailing_window_days`), that step is silently skipped.
#'   \item `calculate_observation_by` is only used when
#'     `calculate_observation_option = "other"`, and is ignored if
#'     `calculate_observation_option` is `NULL`.
#'   \item The leading and trailing steps remove records with
#'     `phenophase_status` of `-1` and keep one record per individual,
#'     phenophase, and date. The conflict-resolution step also removes `NA`
#'     statuses.
#'   \item If no arguments are supplied, the data are returned unchanged.
#'   \item Progress messages are printed for each step that runs. Use
#'     [suppressMessages()] to silence them.
#' }
#'
#' @param data A data frame or tibble of phenophase observations. It must
#'   contain the columns required by each step you request, including
#'   `individual_id`, `phenophase_id`, `observation_date` (class `Date`), and
#'   `phenophase_status` (`1` = yes, `0` = no, `-1` = unknown). Resolving
#'   conflicts also requires `observedby_person_id`, plus any columns named in
#'   `calculate_observation_by`.
#' @param calculate_observation_option A character string, one of `"by_year"`,
#'   `"all_years"`, or `"other"`, specifying how each observer's number of
#'   observations is counted when resolving conflicts. See
#'   [clean_by_resolving_conflict()] for details. If `NULL` (the default),
#'   the conflict-resolution step is skipped.
#' @param calculate_observation_by A character vector of column name(s) used to
#'   group observations when `calculate_observation_option = "other"`.
#'   Defaults to `NULL`.
#' @param min_leading_no A single number giving the minimum `leading_no_count`
#'   a record must have to be kept. Must be supplied together with
#'   `leading_window_days`. Defaults to `NULL` (step skipped).
#' @param leading_window_days A single positive number giving the length, in
#'   days, of the window preceding each observation in which "no" records are
#'   counted. Must be supplied together with `min_leading_no`. Defaults to
#'   `NULL` (step skipped).
#' @param min_trailing_yes A single number giving the minimum
#'   `trailing_yes_count` a record must have to be kept. Must be supplied
#'   together with `trailing_window_days`. Defaults to `NULL` (step skipped).
#' @param trailing_window_days A single positive number giving the length, in
#'   days, of the window following each observation in which "yes" records
#'   are counted. Must be supplied together with `min_trailing_yes`. Defaults
#'   to `NULL` (step skipped).
#' @param min_doy,max_doy Integers giving the lower and upper bounds (inclusive,
#'   day of year, 1-366) of the observations to retain. Either may be `NULL`
#'   to leave that bound open. If both are `NULL` (the default), the window
#'   step is skipped.
#'
#' @return An ungrouped tibble of the retained observations. Depending on
#'   which steps ran, it contains all original columns plus some of:
#'   \describe{
#'     \item{`year`, `num_of_observations`}{Added when conflicts are resolved.}
#'     \item{`leading_no_count`}{Added when the leading "no" step runs.}
#'     \item{`trailing_yes_count`}{Added when the trailing "yes" step runs.}
#'   }
#'
#' @seealso [clean_by_resolving_conflict()], [clean_by_leading_no()],
#'   [clean_by_trailing_yes()], [clean_by_window()]
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   observation_date = as.Date("2024-04-01") + c(0, 2, 2, 4, 6, 8, 10),
#'   observedby_person_id = c(10, 10, 20, 10, 10, 10, 10),
#'   phenophase_status = c(0, 0, 1, 1, 1, 1, 0)
#' )
#'
#' # Resolve observer conflicts only
#' clean_data(obs, calculate_observation_option = "by_year")
#'
#' # Keep records followed by at least 2 "yes" observations within 7 days
#' clean_data(obs, min_trailing_yes = 2, trailing_window_days = 7)
#'
#' # Combine several steps
#' clean_data(
#'   obs,
#'   calculate_observation_option = "all_years",
#'   min_leading_no = 1, leading_window_days = 7,
#'   min_trailing_yes = 1, trailing_window_days = 7,
#'   min_doy = 60, max_doy = 200
#' )
#'
#' @export
clean_data <- function(data,
                       calculate_observation_option = NULL,
                       calculate_observation_by = NULL,
                       min_leading_no = NULL,
                       leading_window_days = NULL,
                       min_trailing_yes = NULL,
                       trailing_window_days = NULL,
                       min_doy = NULL, max_doy = NULL) {
  data_clean <- data

  if (!is.null(calculate_observation_option)) {
    message("Cleaning by resolving conflict")
    data_clean <- data_clean |>
      clean_by_resolving_conflict(
        calculate_observation_option = calculate_observation_option,
        calculate_observation_by = calculate_observation_by
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
