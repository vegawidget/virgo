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

# FIXME: abort for nested calls
population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_point(enc(x = vg_sum(as.numeric(people))))

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = vg_sum(people), y = ordered(age)))

 population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = ordered(age), y = vg_sum(people), color = factor(sex)))

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = ordered(age), y = vg_sum(people), color = factor(sex)),
    opacity = .7, position = "identity")

population %>%
  filter(year == 2000) %>%
  vega(enc(x = ordered(age), y = vg_sum(people), color = factor(sex))) %>%
  mark_bar(position = "fill")

# population %>%
#   filter(year == 2000) %>%
#   vega(enc(x = factor(age), y = vg_sum(people), color = factor(sex))) %>%
#   mark_bar(enc(column = ordered(sex)), position = "dodge")

population %>%
  filter(year == 2000) %>%
  group_by(age, sex) %>%
  mutate(people = sum(people)) %>%
  ggplot(aes(x = factor(age), y = people, fill = factor(sex))) +
  geom_col(position = "dodge")

population %>%
  filter(year == 2000) %>%
  vega() %>%
  mark_bar(enc(x = ordered(age), y = vg_count(age)))
