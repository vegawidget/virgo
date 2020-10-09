library(dplyr)

selection <- select_interval()

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = cyl),
    selection = selection)

selection <- select_interval()

mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = wt, y = mpg),
    selection = c(
      size_if(selection, 180, 60),
      color_if(selection, factor(cyl), "grey")))

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

# vg_filter(selection)
# vg_calculate
p3 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp),
    transform = selection)
hconcat(p1, p3)

p1 %>%
  mark_rule(encoding = enc(y = vg_mean(mpg)), size = 3, color = "red",
    transform = selection)

evt <- "[mousedown[!event.shiftKey], mouseup] > mousemove"
a <- select_interval(on = evt)
b <- select_interval(
  on = "[mousedown[event.shiftKey], mouseup] > mousemove",
  mark = c("fill" =  "#fdbb84", "fillOpacity" = 0.5, "stroke" = "#e34a33"))
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
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = cyl),
    selection = size_if(select_legend(cyl), 100, 20))

mtcars %>%
  mutate(cyl = factor(cyl), gear = factor(gear)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = cyl, shape = gear),
    selection = opacity_if(select_legend(cyl), 1, .2))
