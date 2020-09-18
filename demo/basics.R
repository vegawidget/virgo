library(dplyr)

mtcars %>%
  vega() %>%
  mark_points(encoding = enc(x = wt, y = mpg, color = factor(cyl)))