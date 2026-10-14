#' Resolve conflicting observations by keeping the most experienced observer
#'
#' Cleans phenophase observation data in which multiple observers have
#' reported on the same individual, phenophase, and date. When observers
#' disagree or duplicate each other, the record from the observer with the
#' most total observations (a proxy for experience) is retained. Uncertain
#' and missing status records are removed.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Counts how many observations each observer has made, using the
#'     grouping set by `calculate_observation_option`.
#'   \item Removes observations with an unknown or missing status.
#'   \item When multiple observers recorded the same individual, phenophase,
#'     and date, keeps the record from the observer with the most
#'     observations.
#' }
#'
#' **Choosing how observations are counted.** The `calculate_observation_option`
#' argument controls the groups over which each observer's observations are
#' tallied:
#' \describe{
#'   \item{`"by_year"`}{Counts per observer per year
#'     (`observedby_person_id`, `year`).}
#'   \item{`"all_years"`}{Counts per observer across the entire dataset
#'     (`observedby_person_id` only).}
#'   \item{`"other"`}{Counts per observer within the additional grouping
#'     column(s) given in `calculate_observation_by` (e.g., a site column).
#'     `calculate_observation_by` is required for this option; an error is
#'     thrown if it is `NULL`. `observedby_person_id` is always included
#'     automatically. The derived `year` column may be used here.}
#' }
#'
#' **Important notes:**
#' \itemize{
#'   \item Observations are counted *before* uncertain (`-1`) and `NA` records
#'     are removed, so those records contribute to an observer's total.
#'   \item Ties are preserved (`with_ties = TRUE`). If two observers have the
#'     same `num_of_observations` for a given individual, phenophase, and
#'     date, both records are kept, so the output may still contain more than
#'     one record per date.
#'   \item Records with no conflict (only one observer for that individual,
#'     phenophase, and date) are always retained, provided their status is
#'     known.
#' }
#'
#' @param data A data frame or tibble of phenophase observations. Must contain
#'   the following columns, plus any columns named in
#'   `calculate_observation_by`:
#'   \describe{
#'     \item{`individual_id`}{Identifier for the individual plant or animal.}
#'     \item{`phenophase_id`}{Identifier for the phenophase being observed.}
#'     \item{`observation_date`}{Date of the observation, of class `Date`.}
#'     \item{`observedby_person_id`}{Identifier for the observer.}
#'     \item{`phenophase_status`}{Integer status: `1` = yes, `0` = no,
#'       `-1` = unknown (removed).}
#'   }
#' @param calculate_observation_option A character string specifying how each
#'   observer's number of observations is calculated. One of `"by_year"`
#'   (the default), `"all_years"`, or `"other"`. If `"other"`,
#'   `calculate_observation_by` must also be supplied. See Details.
#' @param calculate_observation_by A character vector of column name(s) in
#'   `data` used to group observations when
#'   `calculate_observation_option = "other"`. Required for `"other"`; an
#'   error is thrown if it is `NULL`. Ignored for the other options.
#'   Defaults to `NULL`.
#'
#' @examples
#' obs <- data.frame(
#'   individual_id = 1,
#'   phenophase_id = 501,
#'   observation_date = as.Date(c(
#'     "2024-04-01", "2024-04-01", "2024-04-02", "2024-04-03", "2024-04-03"
#'   )),
#'   observedby_person_id = c(10, 20, 10, 10, 20),
#'   site_id = "A",
#'   phenophase_status = c(1, 0, 1, 1, 0)
#' )
#'
#' # Default: count observations per observer per year
#' clean_by_resolving_conflict(obs)
#'
#' # Count observations per observer across all years
#' clean_by_resolving_conflict(obs, calculate_observation_option = "all_years")
#'
#' # Count observations per observer within each site
#' clean_by_resolving_conflict(
#'   obs,
#'   calculate_observation_option = "other",
#'   calculate_observation_by = "site_id"
#' )
#'
#' #' # "other" requires calculate_observation_by, so this raises an error
#' try(clean_by_resolving_conflict(obs, calculate_observation_option = "other"))
#'
#' @export
clean_by_resolving_conflict <- function(data, calculate_observation_option = c("by_year", "all_years", "other"), calculate_observation_by = NULL) {
  calculate_observation_option <- match.arg(calculate_observation_option)

  #validate calculate_observation_by when option is "other"
  if(calculate_observation_option == "other" && is.null(calculate_observation_by)) {
    stop(
      "`calculate_observation_by` must be provided when ",
      "`calculate_observation_option = \"other\"`."
    )
  }

  # set which columns to group observation data by
  if (calculate_observation_option == "by_year") {
    group_vars <- c("observedby_person_id", "year")
    # checks if option is "all_years", if true, only group by observer
    } else if (calculate_observation_option == "all_years") {
    group_vars <- c("observedby_person_id")
    # checks if option is "other", combines whatever specified columns into one vector
    } else if (calculate_observation_option == "other") {
    group_vars <- c(calculate_observation_by, "observedby_person_id") |>
    # remove duplicate entries
      unique() |>
      sort()
  }

  data_clean <- data |>
    # change observation_date to proper formatting
    dplyr::mutate(year = lubridate::year(observation_date)) |>
    # count number of observations for each observer
    dplyr::group_by(!!!rlang::syms(group_vars)) |>
    dplyr::mutate(num_of_observations = dplyr::n()) |>
    dplyr::ungroup() |>
    # remove any uncertain observations from the dataframe
    dplyr::filter(phenophase_status != -1, !is.na(phenophase_status)) |>
    # keep the observation from the observer with the most observations
    dplyr::group_by(individual_id, phenophase_id, observation_date) |>
    dplyr::slice_max(num_of_observations, n = 1, with_ties = T) |>
    dplyr::ungroup()

  return(data_clean)
}
