#' @import rlang vctrs vegawidget

new_vegaspec <- function(spec) {
  cls <- c("vegaspec_unit", "vegaspec_vega_lite", "vegaspec", "list")
  structure(spec, class = cls)
}

vega <- function(data = NULL, encoding = enc(), theme = config()) {
  spec <- list(`$schema` = vega_schema(), config = theme) # to be exposed somewhere
  if (!is.null(data)) {
    data <- eval_values(data, encoding)
    spec <- c(spec, list(data = list(values = data)))
  }
  if (!is_empty(encoding)) {
    spec <- c(spec, list(encoding = eval_encoding(data, encoding)))
  }
  new_vegaspec(spec)
}

# selection perhaps can be implemented as delayed reactives
# it will never be evaluated in console mode
# a random id needs to be assigned when it's created for composition

# bind(Year = input_slider(min, max, step, init))
# init = c(Cycliners = 4, Year = 1977)
# init = list(x = c(55, 160), y = c(13, 37))
select_single <- function(encodings = NULL, fields = NULL, init = NULL, bind,
  nearest = FALSE, on = "click", clear = "dblclick", empty = "all", resolve) {

}

select_multi <- function(encodings = NULL, fields = NULL, init = NULL, bind,
  toggle, nearest = FALSE, on = "click", clear = "dblclick", empty = "all",
  resolve) {

}

select_interval <- function(encodings = c("x", "y"), fields = NULL, init = NULL,
  bind, mark, zoom, translate, on = "click", clear = "dblclick", empty = "all",
  resolve) {
  structure(
    list(type = "interval", encodings = encodings),
    name = name, class = "vegaspec_selection")
}

# selection(type = select_interval(mark, translate), encodings, fields, on)
