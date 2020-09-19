library(dplyr)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg, color = cyl)) %>%
  mark_points()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_points(encoding = enc(x = wt, y = mpg, color = cyl))

# not working
# vega() %>%
#   mark_points(encoding = enc(x = wt, y = mpg, color = cyl), data = mtcars)
