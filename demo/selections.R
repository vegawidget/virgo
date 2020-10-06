library(dplyr)

selection <- select_interval()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = cyl),
    selection = selection)

selection <- select_interval()

p1 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = color_if(selection, factor(cyl), "grey"))
p2 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp),
    selection = color_if(selection, factor(cyl), "#99d8c9"))
hconcat(p1, p2)

p3 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp),
    transform = selection)
hconcat(p1, p3)

p1 %>%
  mark_rule(encoding = enc(y = vg_mean(mpg)), size = 3, color = "red",
    transform = selection)

a <- select_interval(on = "[mousedown[!event.shiftKey], mouseup] > mousemove")
b <- select_interval(on = "[mousedown[event.shiftKey], mouseup] > mousemove")
selection_composition(a & b)
selection_composition(a | b)
selection_composition(a | !b)
selection_composition(!(a & b))
selection_composition(!(a & b) & select_interval())


mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = color_if(a | b, factor(cyl), "grey"))

mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = color_if(!(a | b), factor(cyl), "grey"))
