# new `transform` arg https://vega.github.io/vega-lite/docs/loess.html#example
# vega supports a mixture of data and statistical transformations
vega_layer <- function(v, layer = list(), encoding = NULL, data = NULL,
  transform = NULL, selection = NULL) {
  layer_data <- data
  encoding <- c(v$encoding, encoding)
  data <- data %||% v$data$values
  layer <- c(list(width = 300, height = 300), layer)
  if (!is.null(encoding)) {
    layer <- c(layer, list(encoding = eval_encoding(data, encoding)))
  }
  if (!is.null(layer_data)) {
    layer <- c(list(data = list(values = data)), layer)
  }
  if (!is.null(transform)) {
    layer <- c(layer, list(transform = list(list(
      filter = list(selection = selection_composition(transform))))))
  }
  if (!is.null(selection)) {
    if (is_virgo_condition(selection)) {
      condition <- eval_condition(data, selection)
      layer$encoding <- c(layer$encoding, condition)
      selection <- selection$selection
    }
    if (is_virgo_selection(selection)) {
      sel <- unclass(selection)
    }
    layer <- c(layer, list(selection = sel))
  }
  spec <- build_layer(v, add_layer(v$layer, layer))
  new_virgo(spec)
}

build_layer <- function(v, layer) {
  v <- remove_layer(v)
  c(v, list(layer = layer))
}

remove_layer <- function(v) {
  v$layer <- NULL
  v
}

add_layer <- function(layer, new_layer) {
  c(layer, list(new_layer))
}

nlayer <- function(v) {
  length(v$layer)
}

mark_properties <- function(...) {
  dots <- dots_list(..., .named = TRUE, .homonyms = "error")
  if (!(has_name(dots, "tooltip"))) { # enable tooltip by default
    dots$tooltip <- TRUE
  }
  dots
}

# use vega options name but in snake_case
mark_factory <- function(type = "point") {
  force(type)
  function(v, encoding = NULL, data = NULL, transform = NULL,
    selection = NULL, ...) {
    layer <- list(mark = list2(type = type, !!!mark_properties(...)))
    vega_layer(v, layer, encoding, data, transform, selection)
  }
}

mark_arc <- mark_factory(type = "arc")
mark_area <- mark_factory(type = "area")
mark_boxplot <- mark_factory(type = "boxplot")
mark_circle <- mark_factory(type = "point")
mark_ribbon <- mark_errorband <- mark_factory(type = "errorband")
mark_geoshape <- mark_factory(type = "geoshape")
mark_image <- mark_factory(type = "image")
mark_line <- mark_factory(type = "line")
mark_point <- mark_factory(type = "circle")
mark_rect <- mark_factory(type = "rect")
mark_linerange <- mark_rule <- mark_factory(type = "rule")
mark_square <- mark_factory(type = "square")
mark_text <- mark_factory(type = "text")
mark_tick <- mark_factory(type = "tick")
mark_trail <- mark_factory(type = "trail")

mark_bar <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ...) {
  layer <- list(mark = list2(type = "bar", !!!mark_properties(...)))
  v <- vega_layer(v, layer, encoding, data, transform, selection)
  last <- nlayer(v)
  x <- v$layer[[last]]$encoding$x
  v$layer[[last]]$encoding$x$scale$zero <- TRUE
  y <- v$layer[[last]]$encoding$y
  v$layer[[last]]$encoding$y$scale$zero <- TRUE
  v
}

mark_errorbar <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ...) {
  layer <- list(mark = list2(type = "errorbar",
    !!!mark_properties(ticks = TRUE, ...)))
  vega_layer(v, layer, encoding, data, transform, selection)
}

mark_histogram <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ..., bin = TRUE) { # bin = list() opts
  layer <- list(mark = list2(type = "bar", !!!mark_properties(...)))
  v <- vega_layer(v, layer, encoding, data, transform, selection)
  last <- nlayer(v)
  x <- v$layer[[last]]$encoding$x
  v$layer[[last]]$encoding$x <- c(x, list(bin = bin))
  v$layer[[last]]$encoding$y <- list(aggregate = "count")
  v
}

mark_step <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ...) {
  layer <- list(mark = list2(type = "line",
    !!!mark_properties(interpolate = "step-after", ...)))
  vega_layer(v, layer, encoding, data, transform, selection)
}

mark_density <- function() {

}

mark_smooth <- function() {

}

mark_bin2d <- function() {

}

mark_tile <- function() {

}

mark_hline <- function() {

}

mark_vline <- function() {

}

mark_streamgraph <- function() {

}

mark_parcoords <- function() {

}

mark_qq <- function() {

}
