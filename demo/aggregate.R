library(dplyr)

# trying out some nse trickery
population <- jsonlite::read_json(
  "https://vega.github.io/vega-editor/app/data/population.json"
) %>%
  bind_rows()

# mark bar doesn't appear to work not sure why
population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_point(enc(x = sum(people)))

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = sum(people), y = age))

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = age, y = sum(people), color = factor(sex)))
