library(dplyr)

weather <- readr::read_csv("https://vega.github.io/vega-editor/app/data/seattle-weather.csv")

weather %>%
  vega(enc(x = vg_month(date), y = vg_mean(temp_max))) %>%
  mark_line() %>%
  mark_point()

weather %>%
  vega(enc(x = vg_month(date))) %>%
  mark_ribbon(
    enc(y = vg_mean(temp_max), y2 = vg_mean(temp_min)),
    opacity = .3) %>%
  mark_line(enc(y = vg_mean(precipitation)), interpolate = "monotone")

stocks <- readr::read_csv("https://vega.github.io/vega-editor/app/data/stocks.csv")

stocks %>%
  mutate(symbol = factor(symbol)) %>%
  vega(enc(x = vg_year(date), y = vg_mean(price), color = symbol)) %>%
  mark_line() %>%
  mark_point()

data <- jsonlite::fromJSON('[
      {"date": "Sun, 01 Jan 2012 00:00:00", "distance": 1},
      {"date": "Sun, 01 Jan 2012 00:01:00", "distance": 1},
      {"date": "Sun, 01 Jan 2012 00:02:00", "distance": 2},
      {"date": "Sun, 01 Jan 2012 00:03:00", "distance": 1},
      {"date": "Sun, 01 Jan 2012 00:04:00", "distance": 4},
      {"date": "Sun, 01 Jan 2012 00:05:00", "distance": 2},
      {"date": "Sun, 01 Jan 2012 00:06:00", "distance": 5},
      {"date": "Sun, 01 Jan 2012 00:07:00", "distance": 2},
      {"date": "Sun, 01 Jan 2012 00:08:00", "distance": 6},
      {"date": "Sun, 01 Jan 2012 00:09:00", "distance": 4},
      {"date": "Sun, 01 Jan 2012 00:10:00", "distance": 1},
      {"date": "Sun, 01 Jan 2012 00:11:00", "distance": 1},
      {"date": "Sun, 01 Jan 2012 00:12:00", "distance": 3},
      {"date": "Sun, 01 Jan 2012 00:13:00", "distance": 0},
      {"date": "Sun, 01 Jan 2012 00:14:00", "distance": 2},
      {"date": "Sun, 01 Jan 2012 00:15:00", "distance": 3}
    ]')

# FIXME: `step` not working
data %>%
  vega(enc(x = vg_minutes(date, step = 5), y = vg_sum(distance))) %>%
  mark_bar()

library(vegawidget)
list(
  `$schema` = vega_schema(), # specifies Vega-Lite
  data = list(values = data),
  mark = "bar",
  encoding = list(
    x = list(field = "date", timeUnit = list(unit = "minutes"), type = "temporal"),
    y = list(field = "distance", aggregate = "sum", type = "quantitative")
  )
) %>%
  as_vegaspec()
