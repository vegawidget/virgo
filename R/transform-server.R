# Rather than transforming data on the vegaside
# we could compose statistical transformations directly in R
# this would give the option to eventually make them reactive via shiny

# A naive mosaic estimator
stack_count <- function(x, n, id, type = "normalize") {
  stopifnot(is.data.frame(x))
  n <- enquo(n)
  id <- enquo(id)
  type <- arg_match(type, c("normalize", "sum"))

  stack_n <- list(
    left = function(n) cumsum(lag(n, default = 0)),
    right = cumsum
  )

  divider <- identity
  if (type == "normalize") {
    divider <- function(n) sum(n)
  }

  mutate(
    x,
    "stack_count_left_{{id}}" := stack_n$left({{n}}) / divider({{n}}),
    "stack_count_right_{{id}}" := stack_n$right({{n}}) / divider({{n}})
  )

}

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
    stack_count(n = n, id = x) %>%
    mutate(rank_x = dense_rank(!!enc$x)) %>%
    group_by(!!enc$x) %>%
    mutate(
      x = min(stack_count_left_x),
      x2 = max(stack_count_right_x),
      rank_y = dense_rank(!!enc$y),
      distinct_y = n_distinct(!!enc$y)
    ) %>%
    arrange(!!enc$y, .by_group = TRUE) %>%
    stack_count(n = n, id = y )%>%
    ungroup() %>%
    mutate(
      nx = x + (rank_x-1)* 0.01,
      nx2 = x2 + (rank_x - 1) * 0.01,
      ny = stack_count_left_y + (rank_y - 1) * 0.01 + distinct_y * 0.01 / max(distinct_y),
      ny2 = stack_count_right_y + (rank_y - 1) * 0.01 + distinct_y * 0.01 / max(distinct_y),
      xc = (nx + nx2) / 2,
      yc = (ny + ny2) / 2
    )
}
