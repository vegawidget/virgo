library(dplyr)
mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_points(x = wt, y = mpg, color = cyl)
