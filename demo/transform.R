library(dplyr)

# TODO: bad default scale
mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  slice_sample(n = 10) %>%
  mark_point()

dplyr:::slice_sample.data.frame
dplyr:::check_slice_size

selection <- select_interval()

p1 <- mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point(selection = selection)
p2 <- mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  slice_sample(n = 20) %>%
  filter(selection) %>%
  mark_point()
hconcat(p1, p2)

movies <- jsonlite::read_json(
  "https://vega.github.io/vega-editor/app/data/movies.json"
  , simplifyVector = TRUE)

movies %>%
  vega() %>%
  mark_bar(enc(x = vg_argmax(Production_Budget, US_Gross), y = Major_Genre))

mtcars %>%
  vega() %>%
  summarise(mean_acc = vg_mean(mpg)) %>%
  mark_point(enc(x = factor(cyl), y = mean_acc)) %>%
  config_vega() %>%
  as.list()
