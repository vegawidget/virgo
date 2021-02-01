#' Melbourne microclimate measurements
#'
#' @details This data comes from the [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Environment/Microclimate-Sensor-Readings/u4vh-84j8)
#' and contains measurements from microclimate sensors around the city. Here
#' we have restricted the data to contain measurements from January 2020 and June 2020. There are five sites where measurements are taken
#' every 15 minutes.
#'
#' @format A tibble with 32,253 rows and 12 variables:
#' * `site`: Site identifier, this is the location of the weather sensor,
#' there are five sites located around the city.
#' * `site_address`: The address of the site
#' * `longitude, latitude`: The spatial coordinates of the measurement sites
#' * `date_time`: The local date time that a sensor made a recording
#' * `date`: Date associated with `date_time`
#' * `ambient_temperature`: The value of the ambient air temperature in degrees Celsius.
#' * `relative_humidity`: The percent value of the relative humidity (no units)
#' * `barometric_pressure`: The barometric pressure in hectopascals (hPa)
#' * `wind_speed`: The wind speed in kilometers per hour (km/h)
#' * `pm2.5,pm10`: The mass density of particulate matter in the air less than 2.5 (10) micrometers in diameter. Measured in micrograms per cubic meter.
#'
#' @source [Melbourne Open Data Portal](https://data.melbourne.vic.gov.au/Environment/Microclimate-Sensor-Readings/u4vh-84j8)
"melbweather"
