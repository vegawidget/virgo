df <- tibble(x = 0:7, y = 10 ^ x)
df
df %>%
  vega() %>%
  mark_point(enc(x, y)) %>%
  scale_y(domain = c(1 - 0.05, 1e7), type = "log")
