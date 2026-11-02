devtools::load_all()
set.seed(1)

subset_individuals <- function(df, n = 100) {
  keep <- sample(unique(df$individual_id), min(n, dplyr::n_distinct(df$individual_id)))
  dplyr::filter(df, individual_id %in% keep)
}

df_flower <- download_test_data("flower") |> subset_individuals()
df_leaf   <- download_test_data("leaf")   |> subset_individuals()

usethis::use_data(df_flower, df_leaf, overwrite = TRUE, compress = "xz")
