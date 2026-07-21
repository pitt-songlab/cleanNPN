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
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
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
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
    )
    npn_flower_df_2 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 8,
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
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


###

# Download site phenometrics data

data_path <- "alldata/input/NPN/"

species_list <- rnpn::npn_species()

genus_list <- c("Acer", "Quercus", "Betula", "Populus")
year_list <- 1900:(Sys.Date() |> lubridate::year())

for (genusoi in genus_list) {
  message("Starting genus: ", genusoi) # so you can see where it stops

  spid <- species_list |>
    filter(genus == genusoi) |>
    select(species_id) |>
    unique()

  if (nrow(spid) == 0) {
    message("No species found for ", genusoi, ", skipping")
    next
  }

  message("Found ", nrow(spid), " species for ", genusoi)

  # --- Leaf site phenometrics ---
  for (year in year_list) {
    file <- str_c(data_path, "leaf_phenometrics/", genusoi, "/", year, ".csv")

    if (file.exists(file)) {
      message("Skipping (exists): ", file)
      next # jumps to next year
    }

    tryCatch(
      {
        npn_leaf_df <- rnpn::npn_download_site_phenometrics(
          request_source = "YS",
          years = year,
          species_ids = spid$species_id,
          pheno_class_ids = 3,
          num_days_quality_filter = 30,
          additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
        )

        if (!is.null(npn_leaf_df) && nrow(npn_leaf_df) > 0) {
          dir.create(dirname(file), recursive = T, showWarnings = F)
          write.csv(npn_leaf_df, file = file, row.names = FALSE, quote = TRUE)
          print(file)
        }
      },
      error = function(e) {
        message("Skipping leaf ", genusoi, " ", year, ": ", e$message)
      }
    )
  }

  # --- Flower site phenometrics ---
  for (year in year_list) {
    npn_flower_df_1 <- rnpn::npn_download_site_phenometrics(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 7,
      num_days_quality_filter = 30,
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
    )
    npn_flower_df_2 <- rnpn::npn_download_site_phenometrics(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 8,
      num_days_quality_filter = 30,
      additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
    )

    npn_flower_df <- rbind(npn_flower_df_1, npn_flower_df_2)

    if (!is.null(npn_flower_df) && nrow(npn_flower_df) > 0) {
      file <- str_c(data_path, "flower_phenometrics/", genusoi, "/", year, ".csv")
      dir.create(dirname(file), recursive = T, showWarnings = F)
      write.csv(npn_flower_df, file = file, row.names = FALSE, quote = TRUE)
      print(file)
    }
  }
}

###

# Download individual phenometrics

data_path <- "alldata/input/NPN/"

species_list <- rnpn::npn_species()

genus_list <- c("Acer", "Quercus", "Betula", "Populus")
year_list <- 2000:(Sys.Date() |> lubridate::year())

for (genusoi in genus_list) {
  message("Starting genus: ", genusoi)

  spid <- species_list |>
    filter(genus == genusoi) |>
    select(species_id) |>
    unique()

  if (nrow(spid) == 0) {
    message("No species found for ", genusoi, ", skipping")
    next
  }

  message("Found ", nrow(spid), " species for ", genusoi)

  # --- Leaf individual phenometrics ---
  for (year in year_list) {
    file <- str_c(data_path, "leaf_individual_phenometrics/", genusoi, "/", year, ".csv")

    # if (file.exists(file)) {
    #   message("Skipping (exists): ", file)
    #   next
    # }

    tryCatch(
      {
        npn_leaf_df <- rnpn::npn_download_individual_phenometrics(
          request_source = "YS",
          years = year,
          species_ids = spid$species_id,
          pheno_class_ids = 3,
          additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
        )

        if (!is.null(npn_leaf_df) && nrow(npn_leaf_df) > 0) {
          dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
          write.csv(npn_leaf_df, file = file, row.names = FALSE, quote = TRUE)
          message("Saved: ", file)
        }
      },
      error = function(e) {
        message("Skipping leaf ", genusoi, " ", year, ": ", e$message)
      }
    )
  } # end leaf year loop

  # --- Flower individual phenometrics ---
  for (year in year_list) {
    file <- str_c(data_path, "flower_individual_phenometrics/", genusoi, "/", year, ".csv")

    # if (file.exists(file)) {
    #   message("Skipping (exists): ", file)
    #   next
    # }

    tryCatch(
      {
        npn_flower_df <- rnpn::npn_download_individual_phenometrics(
          request_source = "YS",
          years = year,
          species_ids = spid$species_id,
          pheno_class_ids = 7,
          additional_fields = c("ObservedBy_Person_ID", "Site_Name", "Dataset_ID", "Multiple_Observers", "Observed_Status_Conflict_Flag", "Multiple_FirstY")
        )

        if (!is.null(npn_flower_df) && nrow(npn_flower_df) > 0) {
          dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
          write.csv(npn_flower_df, file = file, row.names = FALSE, quote = TRUE)
          message("Saved: ", file)
        }
      },
      error = function(e) {
        message("Skipping flower ", genusoi, " ", year, ": ", e$message)
      }
    )
  } # end flower year loop
} # end genus loop
