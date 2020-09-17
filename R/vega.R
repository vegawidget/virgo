#' @import rlang vctrs vegawidget

vega <- function(data = NULL) {
  structure(list(data = data), class = "vega")
}

mark_points <- function(v, x, y, color) {
  # 1. bare variable
  # 2. ToDo: fun(var)
  x <- enquo(x)
  y <- enquo(y)
  color <- enquo(color)
  x_field <- as_name(x)
  y_field <- as_name(y)
  color_field <- as_name(color)
  x_type <- data_type(eval_tidy(x, data = v$data))
  y_type <- data_type(eval_tidy(y, data = v$data))
  color_type <- data_type(eval_tidy(color, data = v$data))
  encoding <- list(
    x = list(field = x_field, type = x_type),
    y = list(field = y_field, type = y_type),
    color = list(field = color_field, type = color_type)
  )
  structure(list(
    `$schema` = vega_schema(), # to be exposed somewhere
    data = list(values = v$data),
    mark = "point",
    encoding = encoding
  ), class = "vega")
}

#' @export
print.vega <- function(v) {
  print(as_vegaspec(unclass(v)))
}
