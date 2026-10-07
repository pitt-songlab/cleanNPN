#' Clean phenophase observations and count trailing "yes" records
#'
#' Prepares phenology observation data by removing uncertain records,
#' collapsing multiple observations made on the same day into one, and
#' counting how many "yes" observations occur in a forward-looking window
#' after each observation date. The resulting count can be used to judge
#' whether a given observation is supported by subsequent "yes" records
#' (e.g., to filter out isolated or spurious detections).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Removes records where `phenophase_status` is `-1` (unknown/uncertain).
#'   \item Resolves duplicate observations for the same individual,
#'     phenophase, and date by keeping a single record, giving preference to
#'     "yes" (`1`) over "no" (`0`).
#'   \item Groups by `individual_id` and `phenophase_id`, and orders
#'     observations chronologically within each group.
#'   \item Adds `trailing_yes_count`, the number of "yes" observations within
#'     the window beginning the day after the observation date and extending
#'     `trailing_window_days` days past it.
#' }
#'
#' The window is defined relative to each observation's date `d` as
#' `[d + 1, d + trailing_window_days]`. The current observation is therefore
#' **not** included in its own count. Because `complete = FALSE` is used,
#' windows that extend beyond the last observation for an individual are
#' still counted using the observations available, rather than returning
#' `NA`.
#'
#' @param data A data frame or tibble of phenophase observations. Must contain
#'   the following columns:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase being observed.}
#'     \item{`observation_date`}{Date of the observation, of class `Date`.}
#'     \item{`phenophase_status`}{Integer status: `1` = yes, `0` = no,
#'       `-1` = unknown (removed).}
#'   }
#' @param trailing_window_days A single positive number giving the length, in
#'   days, of the forward-looking window used to count subsequent "yes"
#'   observations. Defaults to `7`.
#'
#' @return A tibble (ungrouped) containing the filtered, de-duplicated
#'   observations with all original columns plus:
#'   \describe{
#'     \item{`trailing_yes_count`}{Numeric count of "yes" observations
#'       occurring from 1 to `trailing_window_days` days after each
#'       observation date, within the same `individual_id` and
#'       `phenophase_id`.}
#'   }
#'   Rows are sorted by `individual_id`, `phenophase_id`, and
#'   `observation_date`.
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   observation_date = as.Date("2024-04-01") + c(0, 2, 2, 4, 9),
#'   phenophase_status = c(0, 1, 0, 1, -1)
#' )
#'
#' clean_by_trailing_yes(obs)
#'
#' # Use a shorter window
#' clean_by_trailing_yes(obs, trailing_window_days = 3)
#'
#' @importFrom dplyr filter arrange distinct group_by mutate ungroup desc
#' @importFrom slider slide_index_sum
#' @importFrom lubridate days
#' @export
clean_by_trailing_yes <- function(data, trailing_window_days = 7) {
  data_clean <- data |>
    dplyr::filter(phenophase_status != -1) |>
    # keep one obs per day, yes over no
    dplyr::arrange(individual_id, phenophase_id, observation_date, desc(phenophase_status)) |>
    dplyr::distinct(individual_id, phenophase_id, observation_date, .keep_all = T) |>
    dplyr::group_by(individual_id, phenophase_id) |>
    dplyr::arrange(observation_date, .by_group = TRUE) |>
    # Look forward from tomorrow (.i = obsdate + 1) up to the window limit
    dplyr::mutate(
      trailing_yes_count = slider::slide_index_sum(
        x = (phenophase_status == 1),
        i = observation_date,
        before = -1, # Start counting from tomorrow...
        after = days(trailing_window_days), # ...up until X days from now
        complete = FALSE
      )
    ) |>
    dplyr::ungroup()

  return(data_clean)
}
