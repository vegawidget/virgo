mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  facet_views(row = cyl) %>%
  vega_set_height(height = 900)

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point(data = mtcars) %>%
  facet_views(row = cyl)

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  facet_views(column = cyl)

# different width/height defaults for facets?
mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  facet_views(row = cyl, column = gear)

mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  facet_views(row = cyl, column = gear)

mtcars %>%
  vega(enc(x = ac(c(1,3)))) %>%
  mark_histogram(enc(color = factor(cyl)))

mtcars %>%
  vega(enc(x = ac(c(mpg, wt)), y = ac(c(wt, disp, mpg)))) %>%
  mark_point()

selection <- select_interval()

mtcars %>%
  vega(enc(x = ac(c(mpg, wt)), y = ac(c(wt, disp, mpg)))) %>%
  mark_point(selection = I(selection))

mtcars %>%
  vega(enc(x = ac(c(mpg, wt)), y = ac(c(wt, disp, mpg)))) %>%
  mark_point(enc(color = encode_if(selection, factor(cyl), "black")))




