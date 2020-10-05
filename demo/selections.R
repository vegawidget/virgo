library(dplyr)

selection <- select_interval()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = cyl),
    selection = selection)

selection <- select_interval()

p1 <- mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg), size = 130,
    selection = color_if(selection, cyl, "grey"))
p2 <- mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp), size = 130,
    selection = color_if(selection, cyl, "#99d8c9"))
hconcat(p1, p2)
