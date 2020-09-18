#' @import rlang vctrs vegawidget

cls <- c("vegaspec_unit", "vegaspec_vega_lite", "vegaspec", "list")

vega <- function(data = NULL) {
  structure(list(data = data), class = cls)
}