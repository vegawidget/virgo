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
