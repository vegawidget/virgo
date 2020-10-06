library(dplyr)

# trying out some nse trickery
population <- jsonlite::read_json(
  "https://vega.github.io/vega-editor/app/data/population.json"
) %>%
  bind_rows()

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = vg_sum(people)))


# FIXED
population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(y = vg_sum(people)))

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_point(enc(x = vg_sum(as.numeric(people))))

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = vg_sum(people), y = age))

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = age, y = vg_sum(people), color = factor(sex)))
