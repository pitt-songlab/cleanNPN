calc_first_yes <- function(data) {
  data |> 
    select(-intensity_category_id, -intensity_value, - abundance_value) |> 
    filter(phenophase_status == 1) |> 
    select(-phenophase_status) |> 
    mutate(year = lubridate::year(observation_date) ) |> 
    group_by(individual_id, phenophase_id, year ) |> 
    arrange(day_of_year) |> 
    slice(1) |> 
    ungroup() |> 
    rename(day_of_first_yes = day_of_year) |> 
    select(-observation_date, -observation_id, -update_datetime)
}