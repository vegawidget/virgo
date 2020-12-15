df <- tibble(x = 0:7, y = 10 ^ x)
df %>%
  vega() %>%
  mark_point(enc(x, y)) %>%
  scale_y(type = "log")

df %>%
  vega() %>%
  mark_point(enc(y, x)) %>%
  scale_x(type = "log")

tibble(x = 0:7, y = 2 ^ x) %>%
  vega() %>%
  mark_point(enc(x, y)) %>%
  scale_y(type = "sqrt")

tibble(x = 0:7, y = 2 ^ x) %>%
  vega() %>%
  mark_point(enc(x, y)) %>%
  scale_x(breaks = seq(1, 8, by = 2))

stocks <- readr::read_csv(
  "https://vega.github.io/vega-editor/app/data/stocks.csv"
) %>%
  mutate(date = lubridate::mdy(date))

library(ggplot2)
ggplot(stocks) +
  geom_line(aes(date, price, colour = symbol))

stocks %>%
  vega(enc(x = date, y = price)) %>%
  mark_line(enc(colour = symbol), clip = FALSE) %>%
  scale_x(domain = c(as.Date("2002-01-01"), as.Date("2008-01-01")))

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  scale_x(domain = c(2, 4))

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  scale_x(orient = "top") %>%
  scale_y(orient = "right")

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega(encoding = enc(x = wt, y = mpg)) %>%
  mark_point() %>%
  scale_x(domain = c(2, 4))
