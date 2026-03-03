down_specimen_records <- function(sp) {
  res <- spocc::occ(
    query = sp,
    from = "gbif", has_coords = TRUE, limit = 1e6,
    gbifopts = list(
      hasGeospatialIssue = FALSE,
      basisOfRecord = c(
        "FOSSIL_SPECIMEN",
        "LIVING_SPECIMEN",
        "MATERIAL_SAMPLE",
        "PRESERVED_SPECIMEN"
      )
    )
  )

  dat <- res$gbif$data[[1]] %>%
    mutate(queryname = sp)

  return(dat)
}
