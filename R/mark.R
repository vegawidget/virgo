vega_layer <- function(v, layer = list(), encoding = NULL, data = NULL,
  transform = NULL, selection = NULL) {
  layer_data <- data
  fields <- encoding <- merge_encoding(c(v$encoding, encoding))
  data <- data %||% v$data$values
  if (!is.null(encoding)) {
    layer <- c(layer, list(encoding = eval_encoding(data, encoding)))
  }

  if (!is.null(transform)) {
    layer <- c(layer, list(transform = list(list(
      filter = list(selection = selection_composition(transform))))))
  }
  if (!is.null(selection)) {
    if (is_virgo_condition(selection)) {
      trues <- map(selection, function(x) x$true)
      falses <- map(selection, function(x) x$false)
      fields <- c(fields, trues, falses)
      condition <- eval_condition(data, selection)
      layer$encoding <- c(layer$encoding, condition)
      selection <- selection[[1]]$selection
    }
    if (is_virgo_selection(selection)) {
      sel <- unclass(selection)
    }
    layer <- c(layer, list(selection = sel))
  }

  # data needs updating
  fields <- vec_set_names(fields, map_chr(fields, as_field))
  data <- eval_virgo_mask(data, fields)
  layer <- c(list(data = list(values = data)), layer)

  spec <- build_layer(v, add_layer(v$layer, layer))
  new_virgo(spec)
}

merge_encoding <- function(x) {
  x <- rev(x)
  names_x <- names(x)
  x <- rev(x[vec_match(unique(names_x), names_x)])
  x[!map_lgl(x, quo_is_null)]
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
  dots <- vec_set_names(dots, standardise_names(names(dots)))
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
mark_ribbon <- mark_factory(type = "area")
mark_boxplot <- mark_factory(type = "boxplot")
mark_circle <- mark_factory(type = "point")
mark_errorband <- mark_factory(type = "errorband")
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

position_to_stack <- function(position = "stack") {
  if (position == "identity") {
    FALSE
  } else if (position == "stack") {
    TRUE
  } else if (position == "fill") {
    "normalize"
  # } else if (position == "dodge") {
  #   v$facet$column <- v$layer[[last]]$encoding$column
  #   v$layer[[last]]$encoding$column <- NULL
  #   stack <- FALSE
  } else {
    abort("Oops!")
  }
}

mark_area <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ..., position = "stack") {
  layer <- list(mark = list2(type = "area", !!!mark_properties(...)))
  v <- vega_layer(v, layer, encoding, data, transform, selection)
  last <- nlayer(v)
  v$layer[[last]]$encoding$y$stack <- position_to_stack(position)
  v$layer[[last]]$encoding$y$scale$zero <- TRUE
  v
}

mark_bar <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ..., position = "stack") {
  layer <- list(mark = list2(type = "bar", !!!mark_properties(...)))
  v <- vega_layer(v, layer, encoding, data, transform, selection)
  last <- nlayer(v)
  v$layer[[last]]$encoding$x$scale <- NULL
  v$layer[[last]]$encoding$y$scale <- NULL
  v$layer[[last]]$encoding$y$stack <- position_to_stack(position)
  v
}

mark_errorbar <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ...) {
  layer <- list(mark = list2(type = "errorbar",
    !!!mark_properties(ticks = TRUE, ...)))
  vega_layer(v, layer, encoding, data, transform, selection)
}

mark_histogram <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ..., position = "stack", bin = TRUE) { # bin = list() opts
  v <- mark_bar(v, encoding, data, transform, selection, ...,
    position = position)
  last <- nlayer(v)
  x <- v$layer[[last]]$encoding$x
  y <- v$layer[[last]]$encoding$y
  v$layer[[last]]$encoding$x <- c(x, list(bin = bin))
  v$layer[[last]]$encoding$y <- c(y, aggregate = "count")
  v
}

mark_step <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ...) {
  layer <- list(mark = list2(type = "line",
    !!!mark_properties(interpolate = "step-after", ...)))
  vega_layer(v, layer, encoding, data, transform, selection)
}

mark_density <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ..., position = "identity", density = list()) {
  v <- mark_area(v, encoding, data, transform, selection, ...,
    position = position)
  last <- nlayer(v)
  enc <- v$layer[[last]]$encoding
  density_field <- enc$x$field
  groupby <- as.list(unique(c(enc$color$field, enc$fill$field, enc$detail$field,
    enc$stroke$field)))
  v$layer[[last]]$transform <- list(list2(density = density_field,
    groupby = groupby, !!!density))
  v$layer[[last]]$encoding$x$field <- "value"
  v$layer[[last]]$encoding$x$scale$domain <- NULL
  v$layer[[last]]$encoding$y <- c(enc$y, field = "density", type = "quantitative")
  v
}

mark_smooth <- function() {

}

mark_bin2d <- function(v, encoding = NULL, data = NULL, transform = NULL,
  selection = NULL, ..., bin = TRUE) { # bin = list() opts
  # TODO: `bin` needs to take `x` and `y` bin setup
  layer <- list(mark = list2(type = "rect", !!!mark_properties(...)))
  v <- vega_layer(v, layer, encoding, data, transform, selection)
  last <- nlayer(v)
  x <- v$layer[[last]]$encoding$x
  y <- v$layer[[last]]$encoding$y
  v$layer[[last]]$encoding$x <- c(x, list(bin = bin))
  v$layer[[last]]$encoding$y <- c(x, list(bin = bin))
  v
}

mark_tile <- function() {

}

mark_streamgraph <- function() {

}

mark_parcoords <- function() {

}

mark_qq <- function() {

}
