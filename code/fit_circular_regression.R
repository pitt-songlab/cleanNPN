# fit_circular_regression(df_plant %>% filter( species == "Cirsium horridulum"))

fit_circular_regression <- function(dat) {
  # Convert days of the year to circular data
  angles <- circular((dat$doy - 1) / 365 * 2 * pi)
  years <- dat$year %>%
    cbind(1)

  # Fit circular-linear regression model
  model <- lm.circular(y = angles, x = years, init = c(5, 1), type = "c-l", verbose = T)

  return(model)
}
