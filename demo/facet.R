mtcars %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  facet_views(row = cyl)

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
