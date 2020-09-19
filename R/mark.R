# looks like vegalite doesn't associate `layer` to a specific `data` set
# should be new `transform` arg https://vega.github.io/vega-lite/docs/loess.html#example
vega_layer <- function(v, layer = list(), encoding = NULL, selection = NULL,
  ...) {
  data <- v$data$values
  if (!is.null(encoding)) {
    layer <- c(layer, list(encoding = eval_encoding(data, encoding)))
  }
  if (!is.null(selection)) {
    sel <- list2(!!(selection %@% "name") := unclass(selection))
    layer <- c(layer, list(selection = sel))
  }
  spec <- c(v, list(layer = list(layer)))
  structure(spec, class = cls)
}

mark_points <- function(v, encoding = NULL, selection = NULL, ...) {
  layer <- list(mark = list(type = "point", filled = TRUE))
  vega_layer(v, layer, encoding, selection, ...)
}
