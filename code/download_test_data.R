# download red maple data in one year for leaf and flower
# no argument needed unless it's to toggle phenophase
# output will be a dataframe

# df_test <- download_test_data()

download_test_data <- function () {

}


###
for (genusoi in genus_list) {
  spid <- species_list |>
    filter(genus == genusoi) |>
    select(species_id) |>
    unique()

  for (year in year_list) {
    npn_leaf_df <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 3,
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Naturalist_Skill_Level")
    )

    if (nrow(npn_leaf_df) > 0) {
      file <- str_c(data_path, "leaf", "/", genusoi, "/", year, ".csv")
      dir.create(dirname(file), recursive = T, showWarnings = F)

      write.csv(
        x = npn_leaf_df,
        file = file,
        row.names = FALSE,
        quote = TRUE
      )

      print(file)
    }
  }

  for (year in year_list) {
    npn_flower_df_1 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 7,
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Naturalist_Skill_Level")
    )
    npn_flower_df_2 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 8,
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Naturalist_Skill_Level")
    )
    npn_flower_df <- rbind(npn_flower_df_1, npn_flower_df_2)

    if (nrow(npn_flower_df) > 0) {
      file <- str_c(data_path, "flower", "/", genusoi, "/", year, ".csv")
      dir.create(dirname(file), recursive = T, showWarnings = F)

      write.csv(
        x = npn_flower_df,
        file = file,
        row.names = FALSE,
        quote = TRUE
      )

      print(file)
    }
  }
}

