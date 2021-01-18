## code to prepare `melbweather` dataset goes here
# update once rwalkr gets put up on cran
# remotes::install_github("earowang/rwalkr@d528e7b")

# well we use three months of data for summer 2019/20
library(rwalkr)
library(dplyr)
start <- as.Date("2019-12-01")
end <- as.Date("2020-02-29")

# extract regular measurements, not any averaged forms
# variables are temperature, relative humidity, barometric pressure,
# particular matter 2.5 and 10, and wind speed
sensors <- c("TPH.TEMP", "TPH.RH", "TPH.PRESSURE", "PM2.5", "PM10", "WS")

sensors_clean <- c("ambient_temperature",
                   "relative_humidity",
                   "barometric_pressure",
                   "pm2.5",
                   "pm10",
                   "wind_speed")
names(sensors_clean) <- sensors


melbweather <- melb_weather(start, end)

melbweather <- melbweather %>%
  filter(sensor_type %in% sensors) %>%
  mutate(sensor_type = sensors_clean[sensor_type],
         value = as.numeric(value))

melbweather <- tidyr::pivot_wider(melbweather,
                                  id_cols = c("site", "date_time", "date"),
                                  names_from = sensor_type)
# pull in the sites lat lon
sites <- select(pull_weather_sensors(), site_id, longitude, latitude)

melbweather <- left_join(melbweather, sites, by = c("site" = "site_id"))

melbweather <- melbweather %>%
  select(site, longitude, latitude, date_time, date, everything())

usethis::use_data(melbweather, overwrite = TRUE)
