# Rather than transforming data on the vegaside
# we could compose statistical transformations directly in R
# this would give the option to eventually make them reactive via shiny

# A naive mosaic estimator
transform_mosaic <- function(data, enc) {
  # count x,y encodings
  # arrange by x encoding
  #  stack x
  # find min counts within each x
  # arrange by y
  # stack y
  # create offsets
  data %>%
    count(!!!unname(enc)) %>%
    arrange(!!enc$x) %>%
    mutate(nn = cumsum(lag(n, default = 0)) / sum(n),
           nnn = cumsum(n) / sum(n),
           rank_x = dense_rank(!!enc$x)) %>%
    group_by(!!enc$x) %>%
    mutate(x = min(nn), x2 = max(nnn),
           rank_y = dense_rank(!!enc$y),
           distinct_y = n_distinct(!!enc$y)) %>%
    arrange(!!enc$y, .by_group = TRUE) %>%
    mutate(y = cumsum(lag(n, default = 0)) / sum(n),
           y2 = cumsum(n) / sum(n)) %>%
    ungroup() %>%
    mutate(
      nx = x + (rank_x-1)* 0.01,
      nx2 = x2 + (rank_x - 1) * 0.01,
      ny = y + (rank_y - 1) * 0.01 + distinct_y * 0.01 / max(distinct_y),
      ny2 = y2 + (rank_y - 1) * 0.01 + distinct_y * 0.01 / max(distinct_y),
      xc = (nx + nx2) / 2,
      yc = (ny + ny2) / 2
    )

}
