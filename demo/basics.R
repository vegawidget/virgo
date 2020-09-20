library(dplyr)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg, color = cyl)) %>%
  mark_point()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_point(encoding = enc(x = wt, y = mpg, color = cyl))

vega() %>%
  mark_point(encoding = enc(x = wt, y = mpg), data = mtcars)

vega(data = iris) %>%
  mark_point(encoding = enc(x = wt, y = mpg), data = mtcars)

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

vega() %>%
  mark_ribbon(enc(x = year, y = ymin, y2 = level), data = huron) %>%
  mark_line(enc(x = year, y = ymean), data = huron)

# perhaps need a new S3 class to `print()`
vega(enc(x = year)) %>%
  mark_ribbon(enc(y = ymin, y2 = level), data = huron) %>%
  mark_line(enc(y = ymean), data = huron)

huron %>%
  vega() %>%
  mark_histogram(enc(x = level))

huron %>%
  vega(enc(x = level)) %>%
  mark_histogram() # vegalite should work with this

huron %>%
  vega() %>%
  mark_histogram(enc(x = level), bin = list(maxbins = 2))

df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

df %>%
  vega(enc(x = trt, color = group)) %>%
  mark_linerange(enc(y = lower, y2 = upper))

df %>%
  vega(enc(x = trt, color = group)) %>%
  mark_errorbar(enc(y = lower, y2 = upper))
