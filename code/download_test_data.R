# download red maple data in one year for leaf and flower
# no argument needed unless it's to toggle phenophase
# output will be a dataframe

# df_test <- download_test_data()

download_test_data <- function(phenophase = c("leaf", "flower")) {
  phenophase <- match.arg(phenophase) #ensure no typos that result in silent failure

  spid <- 3 #NPN species ID for Red maple

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
