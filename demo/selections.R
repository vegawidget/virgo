library(dplyr)

selection <- select_interval()

mtcars %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl)),
    selection = selection)

mtcars %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl)),
    selection = select_interval(init = list(x = c(2, 4), y = c(15, 25))))

mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = c(
      size_if(selection, 180, 60),
      color_if(selection, factor(cyl), "grey")))

p1 <- mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point(selection = color_if(selection, factor(cyl), "grey"))
p2 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp),
    selection = color_if(selection, factor(cyl), "#99d8c9"))
hconcat(p1, p2)

# vg_filter(selection)
# vg_calculate
p3 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp),
    transform = selection)
hconcat(p1, p3)

p1 %>%
  mark_rule(encoding = enc(x = NULL, y = vg_mean(mpg)), size = 3, color = "red",
    transform = selection)

p_bar <- mtcars %>%
  vega(enc(x = disp)) %>%
  mark_histogram(selection = selection) %>%
  mark_histogram(transform = selection, colour = "red")
hconcat(p1, p_bar)

evt <- "[mousedown[!event.shiftKey], mouseup] > mousemove"
a <- select_interval(on = evt)
b <- select_interval(
  on = "[mousedown[event.shiftKey], mouseup] > mousemove",
  mark = c("fill" =  "#fdbb84", "fill_opacity" = 0.5, "stroke" = "#e34a33"))
selection_composition(a & b)
selection_composition(a | b)
selection_composition(a | !b)
selection_composition(!(a & b))
selection_composition(!(a & b) & select_interval())
c(color_if(a | b, "red", "grey"), size_if(a | b, 2, 3))
c(color_if(a | b, "red", "grey"), size_if(a & b, 2, 3))

p4 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = color_if(a | b, factor(cyl), "grey"))
p4

mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = color_if(!(a | b), factor(cyl), "grey"))

p5 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp),
    selection = color_if(a | b, factor(cyl), "#99d8c9"))
hconcat(p4, p5)

mtcars %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl)),
    selection = size_if(select_legend(cyl), 100, 20))

mtcars %>%
  mutate(gear = factor(gear)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl), shape = gear),
    selection = opacity_if(select_legend(gear), 1, .2))

mtcars %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl)),
    selection = select_domain())

sp500 <- readr::read_csv("https://vega.github.io/vega-editor/app/data/sp500.csv") %>%
  mutate(date = lubridate::mdy(date))
brush <- select_interval(encodings = "x")
v1 <- sp500 %>%
  vega(enc(x = date, y = price), height = 200) %>%
  mark_area() %>%
  scale_x(name = NULL, domain = brush)
v2 <- sp500 %>%
  vega(enc(x = date, y = price), height = 100) %>%
  mark_area(selection = brush)
vconcat(v1, v2)
