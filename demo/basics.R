library(dplyr)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg, color = cyl)) %>%
  mark_point()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_point(encoding = enc(x = wt, y = mpg, color = cyl))

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point(encoding = enc(color = cyl))

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point(encoding = enc(size = cyl)) # shape (ok)

# not working
# vega() %>%
#   mark_point(encoding = enc(x = wt, y = mpg, color = cyl), data = mtcars)

mtcars %>%
  vega() %>%
  mark_point(encoding = enc(x = wt))

mtcars %>%
  vega() %>%
  mark_point(encoding = enc(y = wt))

mtcars %>%
  vega() %>%
  mark_point(enc(x = wt, y = mpg), size = 160)

mtcars %>%
  vega() %>%
  mark_circle(enc(x = wt, y = mpg), size = 160)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = cyl, y = wt)) %>%
  mark_boxplot(tooltip = FALSE)
# vegalite: boxplot not working with null tooltip and selection

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = cyl, y = wt)) %>%
  mark_bar()

huron <- tibble(
  year = 1875:1972, level = as.vector(LakeHuron),
  ymin = 500, ymean = 550)

huron %>%
  vega(enc(x = year)) %>%
  mark_ribbon(enc(y = ymin, y2 = level)) %>%
  mark_line(enc(y = ymean))
