library(dplyr)

selection <- select_interval()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = cyl),
    selection = selection)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = color_if(selection, cyl, "grey"))
