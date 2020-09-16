#' @import vctrs

vega <- function(data) {

}

new_vega_spec <- function(data = new_data_frame(),
  layers = new_vega_layers()) {
  structure(list(data = data, layers = layers), class = "vega")
}
