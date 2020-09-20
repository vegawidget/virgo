#' @import rlang vctrs vegawidget

new_vegaspec <- function(spec) {
  cls <- c("vegaspec_unit", "vegaspec_vega_lite", "vegaspec", "list")
  structure(spec, class = cls)
}

vega <- function(data = NULL, encoding = enc()) {
  spec <- list(
    `$schema` = vega_schema(), # to be exposed somewhere
    data = list(values = data)
  )
  if (!is_empty(encoding)) {
    spec <- c(spec, list(encoding = eval_encoding(data, encoding)))
  }
  new_vegaspec(spec)
}

select_single <- function() {

}

select_multi <- function() {

}

select_interval <- function(name = "selection", encodings = c("x", "y"),
  fields, on) {
  structure(
    list(type = "interval", encodings = encodings),
    name = name, class = "vegaspec_selection")
}

# selection(type = select_interval(mark, translate), encodings, fields, on)
