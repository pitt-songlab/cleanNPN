#' Clean phenophase observations using a configurable sequence of filters
#'
#' A wrapper that applies up to four cleaning steps to USA-NPN status and
#' intensity observations. Each step is optional and runs only when its
#' arguments are supplied, so you can combine them as needed. Rows that fail
#' the thresholds you set are removed from the output.
#'
#' The steps run in the following order:
#' \enumerate{
#'   \item Resolves conflicting reports from different observers for the same
#'     individual, phenophase, and date, keeping the record from the observer
#'     with the most observations (see [clean_by_resolving_conflict()]).
#'   \item Counts the leading "no" observations before each record and keeps
#'     records that meet `min_leading_no` (see [clean_by_leading_no()]).
#'   \item Counts the trailing "yes" observations after each record and keeps
#'     records that meet `min_trailing_yes` (see [clean_by_trailing_yes()]).
#'   \item Keeps records within the day-of-year window (see
#'     [clean_by_window()]).
#' }
#'
#' **Important notes:**
#' \itemize{
#'   \item Both the leading and trailing counts are calculated after conflict
#'     resolution, so the impact of `include_conflict` is minimal if you have
#'     already resolved conflicts.
#'   \item Progress messages are printed for each step that runs. Use
#'     [suppressMessages()] to silence them.
#' }
#'
#' @param data A data frame or tibble of phenology observations. Must contain
#'   the following columns:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase.}
#'     \item{`observation_date`}{Date of the observation. Must be of class
#'       `Date`.}
#'     \item{`phenophase_status`}{Integer status of the phenophase: `1` = yes,
#'       `0` = no, `-1` = uncertain.}
#'     \item{`observedby_person_id`}{Identifier for the observer. Required when
#'       resolving conflicts.}
#'     \item{`day_of_year`}{Numeric day of year (1-366) of the observation.
#'       Required when filtering by day-of-year window.}
#'   }
#'   Additional columns named in `calculate_observation_by` are also required
#'   when `calculate_observation_option = "other"`.
#' @param calculate_observation_option A character string specifying how each
#'   observer's number of observations is calculated when resolving conflicts.
#'   One of `"by_year"`, `"all_years"`, or `"other"`. See
#'   [clean_by_resolving_conflict()] for details. Defaults to `NULL` (step
#'   skipped).
#' @param calculate_observation_by A character vector of column name(s) used to
#'   group observations when `calculate_observation_option = "other"`. See
#'   [clean_by_resolving_conflict()] for details. Defaults to `NULL`.
#' @param min_leading_no A single number giving the minimum `leading_no_count`
#'   a record must have to be kept. Must be supplied together with
#'   `leading_window_days`. See [clean_by_leading_no()] for details. Defaults
#'   to `NULL` (step skipped).
#' @param leading_window_days A single positive number giving the length, in
#'   days, of the window preceding each observation in which "no" records are
#'   counted. Must be supplied together with `min_leading_no`. See
#'   [clean_by_leading_no()] for details. Defaults to `NULL` (step skipped).
#' @param min_trailing_yes A single number giving the minimum
#'   `trailing_yes_count` a record must have to be kept. Must be supplied
#'   together with `trailing_window_days`. See [clean_by_trailing_yes()] for
#'   details. Defaults to `NULL` (step skipped).
#' @param trailing_window_days A single positive number giving the length, in
#'   days, of the window following each observation in which "yes" records
#'   are counted. Must be supplied together with `min_trailing_yes`. See
#'   [clean_by_trailing_yes()] for details. Defaults to `NULL` (step skipped).
#' @param include_conflict A single logical value specifying whether to count
#'   same-date conflicting "yes"/"no" records in the leading and trailing
#'   window calculations. See [clean_by_leading_no()] and
#'   [clean_by_trailing_yes()] for details. Defaults to `TRUE`. Ignored unless
#'   the leading or trailing step runs.
#' @param min_doy,max_doy Single numbers giving the earliest and latest day of
#'   year (inclusive, 1-366) to keep. See [clean_by_window()] for details.
#'   Either may be `NULL` to leave that bound open. If both are `NULL` (the
#'   default), the window step is skipped.
#'
#' @return A tibble containing the retained observations, with all original
#'   columns plus, depending on which steps ran:
#'   \describe{
#'     \item{`year`}{Added when conflicts are resolved.}
#'     \item{`num_of_observations`}{Added when conflicts are resolved.}
#'     \item{`leading_no_count`}{Added when the leading "no" step runs.}
#'     \item{`is_conflict`}{Added when the trailing "yes" step runs.}
#'     \item{`trailing_yes_count`}{Added when the trailing "yes" step runs.}
#'   }
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
#' # The day-of-year window step requires a `day_of_year` column
#' obs$day_of_year <- as.integer(format(obs$observation_date, "%j"))
#'
#' # Resolve observer conflicts only
#' clean_data(obs, calculate_observation_option = "by_year")
#'
#' # Keep records followed by at least 2 "yes" observations within 7 days
#' clean_data(obs, min_trailing_yes = 2, trailing_window_days = 7)
#'
#' # Include same-date conflicts in the count
#' clean_data(
#'   obs,
#'   min_trailing_yes = 2, trailing_window_days = 7,
#'   include_conflict = TRUE
#' )
#'
#' # Exclude same-date conflicts from the count
#' clean_data(
#'   obs,
#'   min_trailing_yes = 2, trailing_window_days = 7,
#'   include_conflict = FALSE
#' )
#'
#' # Restrict to a day_of_year window (uses the `day_of_year` column)
#' clean_data(obs, min_doy = 95, max_doy = 100)
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
                       include_conflict = TRUE,
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
      clean_by_leading_no(
        leading_window_days = leading_window_days,
        include_conflict = include_conflict
      )
  }

  if (!is.null(trailing_window_days) && !is.null(min_trailing_yes)) {
    message("Cleaning by trailing yes")
    data_clean <- data_clean |>
      clean_by_trailing_yes(
        trailing_window_days = trailing_window_days,
        include_conflict = include_conflict
      )
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
