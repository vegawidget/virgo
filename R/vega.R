#' @import rlang vctrs vegawidget

cls <- c("vegaspec_unit", "vegaspec_vega_lite", "vegaspec", "list")

vega <- function(data = NULL) {
  structure(list(data = data), class = cls)
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