library(tidyverse)

data_path <- "alldata/input/NPN/"

path_leaf <- paste0(data_path, "leaf/")
path_flower <- paste0(data_path, "flower/")

species_list <- rnpn::npn_species()
genus_list <- c(
  "Acer",
  "Quercus",
  "Betula",
  "Populus"
)

year_list <- 1900:(Sys.Date() |> lubridate::year())

###
for (genusoi in genus_list) {
  spid <- species_list %>%
    filter(genus == genusoi) %>%
    select(species_id) %>%
    unique()

  for (year in year_list) {
    npn_leaf_df <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 3
    )

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

  for (year in year_list) {
    npn_flower_df_1 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 7
    )
    npn_flower_df_2 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 8
    )
    npn_flower_df <- rbind(npn_flower_df_1, npn_flower_df_2)

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
