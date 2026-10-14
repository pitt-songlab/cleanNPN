#' Calculate the first "yes" observation per individual, phenophase, and year
#'
#' Reduces NPN Status and Intensity data to a single record for each
#' combination of individual, phenophase, and calendar year: the earliest
#' "yes" observation, as measured by day of year. This is commonly used to
#' derive the onset date of a phenophase (e.g., first leaf, first flower)
#' for each individual in each year.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Drops the `intensity_category_id`, `intensity_value`, and
#'     `abundance_value` columns.
#'   \item Keeps only "yes" records (`phenophase_status == 1`), then drops
#'     `phenophase_status`. Records with a status of `0`, `-1`, or `NA`
#'     are removed.
#'   \item Derives `year` from `observation_date`.
#'   \item Within each `individual_id`, `phenophase_id`, and `year`, sorts
#'     by `day_of_year` and keeps the first row.
#'   \item Renames `day_of_year` to `day_of_first_yes` and drops
#'     `observation_date`, `observation_id`, and `update_datetime`.
#' }
#'
#' **Important notes:**
#' \itemize{
#'   \item Individuals or years with no "yes" observations do not appear in
#'     the output, because the "yes" filter is applied before grouping.
#'   \item If an individual has several "yes" records on the same
#'     `day_of_year` within a year, only one is retained (the first one in
#'     the existing row order), because `slice(1)` is used rather than
#'     `slice_min(with_ties = TRUE)`. Any other columns that differ between
#'     those records (e.g., `observedby_person_id`) will reflect only the
#'     retained row.
#'   \item Because the first "yes" is chosen by `day_of_year` within a
#'     calendar year, it does not account for phenophases that span the
#'     turn of the year.
#' }
#'
#' @param data A data frame or tibble of phenophase observations. Must
#'   contain the following columns:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase being observed.}
#'     \item{`observation_date`}{Date of the observation, of class `Date`.}
#'     \item{`day_of_year`}{Day of year (1-366) of the observation.}
#'     \item{`phenophase_status`}{Integer status: `1` = yes, `0` = no,
#'       `-1` = unknown.}
#'     \item{`intensity_category_id`, `intensity_value`, `abundance_value`}{
#'       Intensity and abundance fields. These are dropped and must be
#'       present for the function to run.}
#'     \item{`observation_id`, `update_datetime`}{Record metadata, dropped
#'       from the output and must be present.}
#'   }
#'   Any other columns are carried through to the output.
#'
#' @return An ungrouped tibble with one row per `individual_id`,
#'   `phenophase_id`, and `year` that has at least one "yes" observation. It
#'   contains all columns of `data` except those removed above, plus:
#'   \describe{
#'     \item{`year`}{Integer calendar year of the first "yes" observation.}
#'     \item{`day_of_first_yes`}{Day of year (renamed from `day_of_year`) of
#'       the earliest "yes" observation in that year.}
#'   }
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = c(1, 1, 1, 1, 2),
#'   phenophase_id = 501,
#'   observation_id = 1:5,
#'   observation_date = as.Date(c(
#'     "2024-04-01", "2024-04-10", "2024-04-15", "2025-03-28", "2024-05-02"
#'   )),
#'   day_of_year = c(92, 101, 106, 87, 123),
#'   phenophase_status = c(0, 1, 1, 1, 1),
#'   intensity_category_id = NA,
#'   intensity_value = NA,
#'   abundance_value = NA,
#'   update_datetime = Sys.time()
#' )
#'
#' calc_first_yes(obs)
#'
#' @export
calc_first_yes <- function(data) {
  data |>
    dplyr::select(-intensity_category_id, -intensity_value, - abundance_value) |>
    dplyr::filter(phenophase_status == 1) |>
    dplyr::mutate(year = lubridate::year(observation_date) ) |>
    dplyr::group_by(individual_id, phenophase_id, year ) |>
    dplyr::arrange(day_of_year) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename(day_of_first_yes = day_of_year) |>
    dplyr::select(-observation_date, -observation_id, -update_datetime)
}
