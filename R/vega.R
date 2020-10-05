#' @import rlang vctrs vegawidget

new_vegaspec <- function(spec) {
  cls <- c("vegaspec_unit", "vegaspec_vega_lite", "vegaspec", "list")
  structure(spec, class = cls)
}

vega <- function(data = NULL, encoding = enc(), theme = config()) {
  spec <- list(
    `$schema` = vega_schema(),
    config = theme
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
  unclassed <- map(lst, unclass)
  spec <- list(`$schema` = vega_schema(), hconcat = list2(!!!unclassed))
  new_vegaspec(spec)
}
