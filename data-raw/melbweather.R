## code to prepare `melbweather` dataset goes here
# update once rwalkr gets put up on cran
# remotes::install_github("earowang/rwalkr@d528e7b")
# well we use January and June 2020
library(rwalkr)
library(dplyr)

extract_measurements <- function(month_rng) {
  # extract regular measurements, not any averaged forms
  # variables are temperature, relative humidity, barometric pressure,
  # particular matter 2.5 and 10, and wind speed
  sensors <- c("TPH.TEMP",
               "TPH.RH",
               "TPH.PRESSURE",
               "PM2.5",
               "PM10",
               "WS")

  sensors_clean <- c("ambient_temperature",
                     "relative_humidity",
                     "barometric_pressure",
                     "pm2.5",
                     "pm10",
                     "wind_speed")

  names(sensors_clean) <- sensors

  start <- month_rng[1]
  end <- month_rng[2]

  # download data from start and end,
  # filter to relevant sensors and pivot to wide form
  melb_weather(start, end) %>%
    filter(sensor_type %in% sensors) %>%
    mutate(
      sensor_type = sensors_clean[sensor_type],
      value = as.numeric(value)
    ) %>%
    tidyr::pivot_wider(
      id_cols = c("site", "date_time", "date"),
      names_from = sensor_type
    )
}

months <- list(
  jan = c(as.Date("2020-01-01"), as.Date("2020-01-31")),
  jun = c(as.Date("2020-06-01"), as.Date("2020-06-30"))
)

melbweather <- bind_rows(lapply(months, extract_measurements))

# pull in the sites coordinates and description
sites <- select(pull_weather_sensors(),
                site_id, description, longitude, latitude)

melbweather <- melbweather %>%
  left_join(sites, by = c("site" = "site_id")) %>%
  select(site,  site_address = description, longitude, latitude, date_time, date, everything())

usethis::use_data(melbweather, overwrite = TRUE)
