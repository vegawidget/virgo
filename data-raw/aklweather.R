## code to prepare `aklweather` dataset goes here
library(tidyverse)
library(lubridate)
# Read stations data
stations <- read_table("data-raw/ghcnd-stations.txt",
  col_names = c("id", "lat", "lon"))

library(ggmap)
akl_box <- c(left = 174.69, bottom = -37.09, right = 174.94, top = -36.60)
akl_map <- get_map(akl_box)
ggmap(akl_map) +
  geom_point(aes(x = lon, y = lat), stations)

akl_id <- stations %>%
  filter(akl_box["left"] < lon, lon < akl_box["right"],
    akl_box["bottom"] < lat, lat < akl_box["top"])

library(rnoaa)
startdate <- make_date(2010) + days(251) * 0:15
enddate <- startdate - days(1)
enddate <- c(enddate[-1], today())
akl_ncdc <- map2_dfr(startdate, enddate, function(x, y) {
  ncdcout <- ncdc(datasetid = "GHCND",
    stationid = paste0("GHCND:", akl_id$id), limit = 1000,
    startdate = x, enddate = y)
  ncdcout$data
})
aklweather <- akl_ncdc %>%
  select(date, datatype, value) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  pivot_wider(names_from = datatype, values_from = value) %>%
  rename_with(tolower) %>%
  mutate(across(!date, ~ . / 10))

usethis::use_data(aklweather, overwrite = TRUE)

akl_weather <- aklweather %>%
  mutate(rain = as.integer(prcp > 0))

timeline <- select_interval("x")
p_time <- akl_weather %>%
  vega(enc(x = date, y = tavg), width = 600, height = 100) %>%
  mark_line(size = 0.5, selection = timeline)
p_avg <- akl_weather %>%
  vega(enc(x = vg_month(date)), width = 600, height = 350) %>%
  mark_ribbon(
    enc(y = vg_mean(tmin), y2 = vg_mean(tmax)),
    interpolate = "monotone",
    colour = "#fc9272", opacity = 0.3,
    transform = timeline) %>%
  mark_line(enc(y = vg_mean(prcp)), colour = "#3182bd",
    transform = timeline) %>%
  mark_point(enc(y = vg_mean(prcp)), colour = "#3182bd",
    transform = timeline) %>%
  resolve_views(scale = list(y = "independent"))
p_rain <- akl_weather %>%
  vega(width = 600, height = 40) %>%
  mark_bar(enc(x= vg_sum(rain)), fill =  "#3182bd",
    transform = timeline) %>%
  scale_x(name = "# of raining days")
vconcat(p_time, p_avg, p_rain)
