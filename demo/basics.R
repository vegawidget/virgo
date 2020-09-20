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
  mark_boxplot()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = cyl, y = wt)) %>%
  mark_bar()
