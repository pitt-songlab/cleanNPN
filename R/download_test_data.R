#' Download example phenology data for red maple from the USA-NPN
#'
#' Downloads a year of individual phenometric-ready status-and-intensity
#' records for red maple (*Acer rubrum*) from the USA National Phenology
#' Network (USA-NPN) using the \pkg{rnpn} package. It is intended for
#' generating example data to demonstrate and test the cleaning functions in
#' this package.
#'
#' @details
#' The query is fixed to the following parameters, so the only choice you make
#' is the phenophase:
#' \itemize{
#'   \item **Species:** red maple (USA-NPN species ID `3`).
#'   \item **Year:** 2025.
#'   \item **Additional fields:** `ObservedBy_Person_ID`, `Site_Name`, and
#'     `Dataset_ID`, requested in addition to the default columns returned
#'     by [rnpn::npn_download_status_data()].
#'   \item **Request source:** `"package_example_dataset_cleanNPN"`, which
#'     identifies the download to the USA-NPN as coming from this package.
#' }
#'
#' The `phenophase` argument selects the USA-NPN phenophase class:
#' \describe{
#'   \item{`"leaf"`}{Phenophase class ID `3`.}
#'   \item{`"flower"`}{Phenophase class ID `7`.}
#' }
#'
#' **Important notes:**
#' \itemize{
#'   \item An internet connection is required, and the download may take some
#'     time depending on the size of the request and the USA-NPN server load.
#'   \item The USA-NPN database is updated continuously, so repeated calls may
#'     return different records (for example, late-submitted or revised
#'     observations).
#'   \item `phenophase` is validated with [match.arg()], so an unrecognized
#'     value produces an error rather than a silent failure. Partial matching
#'     is allowed (e.g., `"lea"`).
#' }
#'
#' @param phenophase A character string specifying which phenophase class to
#'   download. One of `"leaf"` (the default) or `"flower"`.
#'
#' @return A data frame of status-and-intensity observations as returned by
#'   [rnpn::npn_download_status_data()], with one row per observation record.
#'   It includes the standard USA-NPN columns (such as `individual_id`,
#'   `phenophase_id`, `observation_date`, `day_of_year`, and
#'   `phenophase_status`) plus `observedby_person_id`, `site_name`, and
#'   `dataset_id`.
#'
#' @seealso [rnpn::npn_download_status_data()], [clean_data()]
#'
#' @examples
#' \dontrun{
#' # Requires an internet connection
#' leaf_obs <- download_test_data("leaf")
#' head(leaf_obs)
#'
#' flower_obs <- download_test_data("flower")
#'
#' # Use the downloaded data with the cleaning functions
#' cleaned <- clean_data(
#'   leaf_obs,
#'   calculate_observation_option = "by_year",
#'   min_trailing_yes = 1, trailing_window_days = 7
#' )
#' }
#'
#' @export
download_test_data <- function(phenophase = c("leaf", "flower")) {
  phenophase <- match.arg(phenophase) # ensure no typos that result in silent failure

  spid <- 3 # NPN species ID for Red maple

  pheno_class_id <- switch(
    phenophase,
    leaf = 3,
    flower = 7
  )

    rnpn::npn_download_status_data(
      request_source = "package_example_dataset_cleanNPN",
      years = 2025,
      species_ids = spid,
      pheno_class_ids = pheno_class_id,
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID")
    )
}
