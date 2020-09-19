library(dplyr)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_points(encoding = enc(x = wt, y = mpg, color = cyl))