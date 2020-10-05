#' @import rlang vctrs vegawidget

new_vegaspec <- function(spec) {
  cls <- c("vegaspec_unit", "vegaspec_vega_lite", "vegaspec", "list")
  structure(spec, class = cls)
}

vega <- function(data = NULL, encoding = enc()) {
  spec <- list(
    `$schema` = vega_schema(),
    config = config()
  ) # to be exposed somewhere
  if (!is.null(data)) {
    data <- eval_values(data, encoding)
    spec <- c(spec, list(data = list(values = data)))
  }
  if (!is_empty(encoding)) {
    spec <- c(spec, list(encoding = eval_encoding(data, encoding)))
  }
  new_vegaspec(spec)
}

hconcat <- function(...) {
  lst <- list(...)
  spec <- list(
    `$schema` = vega_schema(),
    config = config(),
    hconcat = list2(!!!lst))
  new_vegaspec(spec)
}
