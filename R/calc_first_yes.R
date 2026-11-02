#' Calculate the day of first "yes" observation
#'
#' For each combination of individual, phenophase, and calendar year, finds
#' the day of year of the earliest observation in which the phenophase was
#' recorded as present (a "yes"). This is commonly used to estimate the onset
#' date of a phenophase such as first leaf, first flower, or first fruit.
#'
#' @details
#' **Important notes:**
#' \itemize{
#'   \item Because the first "yes" is chosen based on day of year within a
#'     calendar year, it does not account for phenophases that span the turn
#'     of the year.
#'   \item Only "yes" records are considered. Records that are "no" or
#'     "uncertain", and records with a missing status, are dropped before the
#'     first observation is selected. As a result, combinations of individual,
#'     phenophase, and year without a "yes" record do not appear in the
#'     output.
#'   \item The result is unaffected by having multiple "yes" records from
#'     different observers for the same individual on the same day.
#' }
#'
#' @param data A data frame or tibble of phenophase observations. Must contain
#'   the following columns:
#'   \describe{
#'     \item{individual_id}{Identifier for the individual plant or animal.}
#'     \item{phenophase_id}{Identifier for the phenophase.}
#'     \item{observation_date}{Date of the observation. Must be of class
#'       `Date` (or otherwise compatible with [lubridate::year()]).}
#'     \item{`day_of_year`}{Numeric day of year (1 to 366) of the
#'       observation.}
#'     \item{`phenophase_status`}{Integer status of the phenophase: `1` = yes,
#'       `0` = no, `-1` = uncertain.}
#'   }
#'
#' @return A tibble with one row per combination of `individual_id`,
#'   `phenophase_id`, and `year`, and the following columns:
#'   \describe{
#'     \item{`phenophase_id`}{Identifier for the phenophase.}
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`year`}{Calendar year of `observation_date`.}
#'     \item{`day_of_first_yes`}{The day of year of the earliest "yes"
#'       observation for the same `individual_id` and `phenophase_id` within
#'       the calendar year.}
#'   }
#'
#' @examples
#' obs <- data.frame(
#'   observation_id = 1:5,
#'   individual_id = c(101, 101, 101, 101, 102),
#'   phenophase_id = 373,
#'   observation_date = as.Date(c(
#'     "2023-04-10", "2023-04-03", "2023-04-17", "2024-03-28", "2023-04-12"
#'   )),
#'   day_of_year = c(100, 93, 107, 88, 102),
#'   phenophase_status = c(1, 1, 1, 1, 0)
#' )
#'
#' calc_first_yes(obs)
#'
#' @export
calc_first_yes <- function(data) {
  data |>
    dplyr::as_tibble() |>
    dplyr::select(individual_id, phenophase_id, observation_date, day_of_year, phenophase_status) |>
    dplyr::filter(phenophase_status == 1) |>
    dplyr::mutate(year = lubridate::year(observation_date)) |>
    dplyr::group_by(individual_id, phenophase_id, year) |>
    dplyr::arrange(day_of_year) |>
    dplyr::slice(1) |>
    dplyr::rename(day_of_first_yes = day_of_year) |>
    dplyr::ungroup() |>
    dplyr::select(phenophase_id, individual_id, year, day_of_first_yes)
}
