library(dplyr)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg, fill = cyl, fill_opacity = gear)) %>%
  mark_point()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg, fill = cyl, fill_opacity = gear)) %>%
  mark_point() %>%
  config_vega()

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg, fill = factor(cyl))) %>%
  mark_point(fill_opacity = .3)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg, group = cyl)) %>%
  mark_line()

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg, tooltip = wt)) %>%
  mark_point()

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg, tooltip = c(wt, cyl))) %>%
  mark_point()

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg, color = factor(cyl))) %>%
  mark_point()

mtcars %>%
  mutate(cyl = factor(cyl, levels = c(8, 6, 4))) %>%
  vega(encoding = enc(x = wt, y = mpg, color = cyl)) %>%
  mark_point()

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg,
    color = factor(cyl, levels = c(8, 6, 4)))) %>%
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

mtcars %>%
  vega() %>%
  mark_point(encoding = enc(x = wt))

mtcars %>%
  vega() %>%
  mark_point(encoding = enc(y = wt))

mtcars %>%
  vega(enc(x = wt, y = mpg, colour = factor(cyl))) %>%
  mark_point() %>%
  scale_colour(name = "Cylinders",
    range = c("purple", "#ff0000", "teal"))

mtcars %>%
  vega(enc(x = wt, y = mpg, colour = factor(cyl))) %>%
  mark_point() %>%
  scale_colour(scheme = "category20b")

mtcars %>%
  vega(enc(x = wt, y = mpg, colour = hp)) %>%
  mark_point() %>%
  scale_colour(range = "diverging", domain_mid = 300)

mtcars %>%
  vega() %>%
  mark_point(enc(x = wt, y = mpg), size = 160)

mtcars %>%
  vega() %>%
  mark_circle(enc(x = wt, y = mpg), size = 160)

mtcars %>%
  vega() %>%
  mark_square(enc(x = wt, y = mpg))

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_tick(enc(x = wt, y = cyl))

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = cyl, y = wt)) %>%
  mark_boxplot(tooltip = FALSE)
# vegalite: boxplot not working with null tooltip and selection

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = cyl, y = vg_count())) %>%
  mark_bar()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, colour = cyl)) %>%
  mark_density()

mtcars %>%
  vega(encoding = enc(x = wt)) %>%
  mark_density()

huron <- tibble(
  year = 1875:1972, level = as.vector(LakeHuron),
  ymin = 500, ymean = 550)

huron %>%
  vega(enc(x = year)) %>%
  mark_ribbon(enc(y = ymin, y2 = level)) %>%
  mark_line(enc(y = 550), colour = "red")

vega() %>%
  mark_ribbon(enc(x = year, y = ymin, y2 = level), data = huron) %>%
  mark_line(enc(x = year, y = ymean), data = huron)

vega(enc(x = year), data = huron) %>%
  mark_ribbon(enc(y = ymin, y2 = level)) %>%
  mark_rule(enc(x = NULL, y = ymean)) # use NULL to not inherit encoding

vega() %>%
  mark_ribbon(enc(x = year, y = ymin, y2 = level), data = huron)

# FIXED
vega(encoding = enc(x = year)) %>%
  mark_ribbon(enc(y = ymin, y2 = level), data = huron) %>%
  mark_line(enc(y = ymean), data = huron)

huron %>%
  vega() %>%
  mark_histogram(enc(x = level))

# FIXED
huron %>%
  vega(enc(x = level)) %>%
  mark_histogram()

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

library(ggplot2)
recent <- economics[economics$date > as.Date("2013-01-01"), ]
vega(recent, enc(date, unemploy)) %>%
  mark_step()

# scale_x(domain, type = "band", range)
# scale_color(domain, type = "category", range)

industries <- jsonlite::read_json(
  "https://vega.github.io/vega-editor/app/data/unemployment-across-industries.json"
) %>%
  bind_rows()
industries %>%
  vega() %>%
  mark_streamgraph(
    enc(x = vg_yearmonth(date), y = vg_sum(count), colour = series))
