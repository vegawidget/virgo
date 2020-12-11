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
    data = selection)


vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl)),
    data = mtcars, selection = selection)

mtcars %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl)),
    selection = I(selection))

mtcars %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl)),
    selection = select_interval(init = list(x = c(2, 4), y = c(15, 25))))


mtcars %>%
  vega() %>%
  mark_point(enc(
    x = wt, y = mpg,
    colour = encode_if(selection, factor(cyl), "grey")))

mtcars %>%
  vega() %>%
  mark_point(enc(
    x = wt, y = mpg,
    colour = encode_if(selection, factor(cyl), "grey"),
    size = encode_if(selection, 180, 60)))

p1 <- mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point(enc(colour = encode_if(selection, factor(cyl), "grey")))
p2 <- mtcars %>%
  vega() %>%
  mark_point(enc(
    x = disp, y = hp,
    colour = encode_if(selection, factor(cyl), "#99d8c9")))
hconcat(p1, p2)

p3 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp),
    selection = selection)
hconcat(p1, p3)

p1 %>%
  mark_rule(
    encoding = enc(x = NULL, y = vg_mean(mpg), colour = factor(cyl)),
    size = 3, selection = selection)
p1 %>%
  mark_rule(
    encoding = enc(x = NULL, y = vg_mean(mpg), colour = factor(cyl)),
    size = 3, data = selection)
p1 %>%
  mark_rule(
    encoding = enc(x = NULL, y = vg_mean(mpg), colour = factor(cyl)),
    size = 3, selection = selection)

p1 %>%
  mark_rule(
    encoding = enc(x = NULL, y = vg_mean(mpg), colour = factor(cyl)),
    size = 3, data = mtcars[1:10, ], selection = selection)

p1 %>%
  mark_rule(
    encoding = enc(x = NULL, y = vg_mean(mpg), colour = factor(cyl)),
    size = 3, data = mtcars[1:10, ], selection = selection)

p1 %>%
  mark_rule(
    encoding = enc(x = NULL, y = vg_mean(mpg), colour = factor(cyl)),
    size = 3, data = mtcars[1:10, ])
# vg_window()

p_bar <- mtcars %>%
  vega(enc(x = disp)) %>%
  mark_histogram() %>%
  mark_histogram(selection = selection, colour = "red")
hconcat(p1, p_bar)

p_box <- mtcars %>%
  vega(enc(x = factor(cyl), y = mpg)) %>%
  mark_boxplot() %>%
  mark_boxplot(selection = selection, colour = "red")
hconcat(p1, p_box)

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
  mark_point(enc(
    x = wt, y = mpg,
    color = encode_if(a | b, factor(cyl), "grey")))
p4

mtcars %>%
  vega() %>%
  mark_point(enc(
    x = wt, y = mpg,
    color = encode_if(!(a | b), factor(cyl), "grey")))

mtcars %>%
  vega() %>%
  mark_point(enc(
    x = wt, y = mpg,
    color = encode_if(a, factor(cyl), "grey"),
    size = encode_if(b, 180, 60)))

p5 <- mtcars %>%
  vega() %>%
  mark_point(
    encoding = enc(x = disp, y = hp,
    color = encode_if(a | b, factor(cyl), "#99d8c9")))
hconcat(p4, p5)

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = cyl,
      tooltip = c(hp, cyl),
      size = encode_if(select_legend(cyl), 100, 20)))

mtcars %>%
  mutate(gear = factor(gear)) %>%
  vega() %>%
  mark_circle(
    encoding = enc(x = wt, y = mpg, color = factor(cyl), shape = gear,
      opacity = encode_if(select_legend(gear), 1, .2)))

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
  mark_area(selection = I(brush))
vconcat(v1, v2)

# input element binding

select_cyl <-
  select_bind(cyl = input_radio(choices = levels(factor(mtcars$cyl))),
              id = "Cylinders")

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    enc(x = hp, y = mpg,
    color = encode_if(select_cyl, cyl, "black")
  ))

select_cyl <-
  select_bind(cyl = input_select(choices = levels(factor(mtcars$cyl))),
              id = "Cylinders")

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_circle(
    enc(x = hp, y = mpg,
    color = encode_if(select_cyl, cyl, "black")
  ))

slider <- select_bind(
  carb = input_slider(min = 1, max = 8, step = 1)
)


mtcars %>%
  vega() %>%
  mark_circle(
    enc(x = hp, y = mpg, colour = encode_if(slider, factor(cyl), "grey"))
  )

slider <- select_bind(
  carb = input_slider(min = 1, max = 8, step = 1, init = 8)
)
mtcars %>%
  vega() %>%
  mark_circle(
    enc(x = hp, y = mpg, color = encode_if(slider, factor(cyl), "grey"))
  )

double_slider <- select_bind(
  cyl = input_select(choices = levels(factor(mtcars$cyl)))
)

# FIXME: does not quite work how i thought it would...
mtcars %>%
  vega() %>%
  mark_circle(
    enc(x = hp, y = mpg,
      colour = encode_if(slider | double_slider, factor(cyl), "grey"),
      size = encode_if(slider | double_slider, 100, 50)))

stocks <- readr::read_csv(
  "https://vega.github.io/vega-editor/app/data/stocks.csv"
) %>%
  mutate(date = lubridate::mdy(date))

hover <- select_single(on = "mouseover", empty = "all", init = list(symbol = "AAPL"))
stocks %>%
  filter(symbol != "IBM") %>%
  vega(enc(x = date, y = price)) %>%
  mark_line(enc(
    colour = encode_if(hover, symbol, "grey"),
    opacity = encode_if(hover, 1, 0.2))) %>%
  mark_text(
    encoding = enc(x = vg_max(date), y = vg_argmax(price, date), text = symbol),
    dx = 4, align = "left")
