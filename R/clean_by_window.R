#' Restrict observations to a day-of-year window
#'
#' Keeps only the observations whose `day_of_year` falls within a specified
#' range. This is useful for excluding records outside the part of the year
#' in which a phenophase is biologically plausible (for example, removing
#' leaf-out records from late autumn). Either bound may be left open.
#'
#' @details
#' Both bounds are inclusive: a record is kept if
#' `day_of_year >= min_doy` and `day_of_year <= max_doy`. If `min_doy` is
#' `NULL`, no lower limit is applied, and if `max_doy` is `NULL`, no upper
#' limit is applied. If both are `NULL`, all records with a non-missing
#' `day_of_year` are returned.
#'
#' **Important notes:**
#' \itemize{
#'   \item Records with `NA` in `day_of_year` are always removed, because
#'     comparisons with `NA` are not `TRUE` and [dplyr::filter()] drops
#'     those rows. This happens even when both bounds are `NULL`.
#'   \item The window is a simple range on a single calendar year's day
#'     numbering, so it cannot represent a window that wraps around the
#'     turn of the year (e.g., day 330 to day 30). If `min_doy > max_doy`,
#'     no rows are returned.
#'   \item Day of year runs from 1 to 366, with 366 occurring only in leap
#'     years. Because the same day of year corresponds to different calendar
#'     dates in leap and non-leap years, dates after February can differ by
#'     one day between years.
#' }
#'
#' @param data A data frame or tibble of phenophase observations. Must contain
#'   a numeric `day_of_year` column.
#' @param min_doy A single number giving the earliest day of year (inclusive)
#'   to keep. If `NULL` (the default), there is no lower limit.
#' @param max_doy A single number giving the latest day of year (inclusive)
#'   to keep. If `NULL` (the default), there is no upper limit.
#'
#' @return A data frame or tibble of the same class as `data`, containing
#'   only the rows with `day_of_year` between `min_doy` and `max_doy`. All
#'   columns are unchanged.
#'
#' @seealso [clean_data()], which calls this function as its final step.
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   day_of_year = c(10, 60, 100, 150, 200, 300),
#'   phenophase_status = c(0, 0, 1, 1, 1, 0)
#' )
#'
#' # Keep days 60 to 200 (inclusive)
#' clean_by_window(obs, min_doy = 60, max_doy = 200)
#'
#' # Only a lower limit
#' clean_by_window(obs, min_doy = 100)
#'
#' # Only an upper limit
#' clean_by_window(obs, max_doy = 100)
#'
#' @export
clean_by_window <- function(data, min_doy = NULL, max_doy = NULL) {
  if (is.null(min_doy)) {
    min_doy <- -Inf
  }
  if (is.null(max_doy)) {
    max_doy <- Inf
  }
  data_clean <- data |>
    dplyr::filter(
      day_of_year >= min_doy,
      day_of_year <= max_doy
    )

  return(data_clean)
}
