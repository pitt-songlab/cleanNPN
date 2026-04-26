tidy_specimen_records <- function(dat, option) {
  if (nrow(dat) == 0) {
    return(NULL)
  }
  if (option == "pollinator") {
    dat_clean <- dat %>%
      select(species = queryname, basisOfRecord, latitude, longitude, year, date = eventDate) %>%
      mutate(doy = lubridate::yday(date)) %>%
      # filter(doy !=1) %>%
      arrange(year) # %>%
    # filter(latitude >0) # northern hemisphere
  }

  if (option == "flower") {
    dat_clean <- dat %>%
      select(species = queryname, basisOfRecord, latitude, longitude, year, date = eventDate, reproductiveCondition) %>%
      mutate(doy = lubridate::yday(date)) %>%
      # filter(doy !=1) %>%
      arrange(year) %>%
      drop_na(reproductiveCondition) %>%
      filter(str_detect(reproductiveCondition %>% tolower(), "fl")) %>%
      filter(!str_detect(reproductiveCondition %>% tolower(), "no fl")) # %>%
    # filter(latitude >0) # northern hemisphere
  }

  return(dat_clean)
}
