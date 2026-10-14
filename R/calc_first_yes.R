#' Calculate the first "yes" observation for each individual, phenophase, and year
#'
#' For each combination of individual plant or animal, phenophase, and calendar
#' year, finds the day of year of the earliest observation in which the
#' phenophase was recorded as present (a "yes"). This is commonly used to
#' estimate the onset date of a phenophase such as first leaf, first flower,
#' or first fruit.
#'
#' Only records with `phenophase_status == 1` are considered. Records with
#' other values (e.g., `0` for "no" or `-1` for "uncertain") and records with
#' missing status are dropped before the first observation is selected. As a
#' result, individual-phenophase-year combinations with no "yes" records do not
#' appear in the output.
#'
#' If an individual has multiple "yes" records on the
#' same day of year within a group, they share the same `day_of_first_yes`, so
#' the result is unaffected by which one is kept.
#'
#' @param data A data frame or tibble of phenology observations. Must contain
#'   the following columns:
#'   \describe{
#'     \item{individual_id}{Identifier for the individual organism.}
#'     \item{phenophase_id}{Identifier for the phenophase.}
#'     \item{observation_date}{Date of the observation. Must be of class
#'       `Date` (or otherwise compatible with [lubridate::year()]).}
#'     \item{day_of_year}{Numeric day of year (1-366) of the observation.}
#'     \item{phenophase_status}{Numeric status of the phenophase, where `1`
#'       indicates "yes" (phenophase observed).}
#'   }
#'   Any additional columns are discarded.
#'
#' @return A tibble with one row per `individual_id`, `phenophase_id`, and
#'   `year` combination, with the following columns:
#'   \describe{
#'     \item{day_of_first_yes}{Day of year of the earliest "yes" observation
#'       (renamed from `day_of_year`).}
#'   }
#'
#' @examples
#' obs <- data.frame(
#'   observation_id = 1:5,
#'   species_id = 3,
#'   individual_id = c(101, 101, 101, 101, 102),
#'   phenophase_id = 373,
#'   phenophase_description = "Leaves",
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
    dplyr::select(individual_id, phenophase_id, observation_date, day_of_year, phenophase_status) |>
    dplyr::filter(phenophase_status == 1) |>
    dplyr::mutate(year = lubridate::year(observation_date) ) |>
    dplyr::group_by(individual_id, phenophase_id, year ) |>
    dplyr::arrange(day_of_year) |>
    dplyr::slice(1) |>
    dplyr::rename(day_of_first_yes = day_of_year) |>
    dplyr::ungroup () |>
    dplyr::select(phenophase_id, individual_id, year, day_of_first_yes)
}
