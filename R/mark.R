# looks like vegalite doesn't associate `layer` to a specific `data` set
# should be new `transform` arg https://vega.github.io/vega-lite/docs/loess.html#example
vega_layer <- function(v, layer = list(), encoding = NULL, transform = NULL,
  selection = NULL) {
  data <- v$data$values
  if (!is.null(encoding)) {
    layer <- c(layer, list(encoding = eval_encoding(data, encoding)))
  }
  if (!is.null(selection)) {
    sel <- list2(!!(selection %@% "name") := unclass(selection))
    layer <- c(layer, list(selection = sel))
  }
  spec <- build_layer(v, add_layer(v$layer, layer))
  new_vegaspec(spec)
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

mark_properties <- function(...) {
  dots <- dots_list(..., .named = TRUE, .homonyms = "error")
  if (!("tooltip" %in% names(dots))) { # enable tooltip by default
    dots$tooltip <- TRUE
  }
  dots
}

mark_factory <- function(type = "point") {
  force(type)
  function(v, encoding = NULL, transform = NULL, selection = NULL, ...) {
    layer <- list(mark = list2(type = type, !!!mark_properties(...)))
    vega_layer(v, layer, encoding, transform, selection)
  }
}

mark_arc <- mark_factory(type = "arc")
mark_area <- mark_factory(type = "area")
mark_bar <- mark_factory(type = "bar")
mark_boxplot <- mark_factory(type = "boxplot")
mark_circle <- mark_factory(type = "circle")
mark_errorband <- mark_factory(type = "errorband")
mark_errorbar <- mark_factory(type = "errorbar")
mark_geoshape <- mark_factory(type = "geoshape")
mark_image <- mark_factory(type = "image")
mark_line <- mark_factory(type = "line")
mark_point <- mark_factory(type = "point")
mark_rect <- mark_factory(type = "rect")
mark_rule <- mark_factory(type = "rule")
mark_square <- mark_factory(type = "square")
mark_text <- mark_factory(type = "text")
mark_tick <- mark_factory(type = "tick")
mark_trail <- mark_factory(type = "trail")

mark_ribbon <- mark_factory(type = "errorband")

# marks <- c("arc", "area", "bar", "boxplot", "circle", "errorband", "errorbar",
#   "geoshape", "image", "line", "rect", "rule", "square", "text", "tick",
#   "trail")
# for (type in marks) {
#   assign(paste0("mark_", type), mark_factory(type = type))
# }
