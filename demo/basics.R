library(dplyr)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg, color = cyl)) %>%
  mark_points()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_points(encoding = enc(x = wt, y = mpg, color = cyl))

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_points(encoding = enc(color = cyl))

# not working
# vega() %>%
#   mark_points(encoding = enc(x = wt, y = mpg, color = cyl), data = mtcars)
