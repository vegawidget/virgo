mark_points <- function(v, encoding = NULL, data = NULL, selection = NULL,
  ...) {
  data <- data %||% v$data
  x <- encoding$x
  y <- encoding$y
  color <- encoding$color
  x_field <- as_name(x)
  y_field <- as_name(y)
  color_field <- as_name(color)
  x_type <- data_type(eval_tidy(x, data = data))
  y_type <- data_type(eval_tidy(y, data = data))
  color_type <- data_type(eval_tidy(color, data = data))
  encoding <- list(
    x = list(field = x_field, type = x_type),
    y = list(field = y_field, type = y_type),
    color = list(field = color_field, type = color_type)
  )
  spec <- list(
    `$schema` = vega_schema(), # to be exposed somewhere
    data = list(values = data),
    mark = "point",
    encoding = encoding
  )
  if (!is.null(selection)) {
    sel <- list2(!!(selection %@% "name") := unclass(selection))
    spec <- c(spec, list(selection = sel))
  }
  structure(spec, class = cls)
}
