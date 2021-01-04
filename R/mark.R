vega_layer <- function(v, layer = list(), encoding = NULL, data = NULL,
  selection = NULL) {
  fields <- encoding <- merge_encoding(c(v$encoding, encoding))
  is_data_inherit <- is.null(data)
  data <- data %||% v$data$values
  if (!is.null(selection)) {
    if (inherits(selection, "AsIs")) {
      layer <- c(layer, list(selection = unclass(selection)))
    } else {
      filter <- list(filter = list(selection = selection_composition(selection)))
      trans <- selection %@% "transform"
      if (is.null(trans)) {
        trans_spec <- list(filter)
      } else {
        new_vars <- map_chr(trans, function(x) x$as)
        old_vars <- map_chr(trans, function(x) x$field)
        for (i in seq_along(new_vars)) { 
          data[[new_vars[i]]] <- eval_tidy(parse_expr(old_vars[i]), data)
        }
        trans_res <- map(trans, function(x) x[["x"]])
        trans_spec <- list(filter, vec_c(!!!trans_res))
      }
      layer <- c(layer, 
        list(selection = unclass(selection)),
        list(transform = trans_spec))
    }
  }

  if (!is.null(encoding)) {
    which_selection <- map_lgl(encoding, function(x) quo_is_call(x, "encode_if"))
    encoding_sel <- encoding[which_selection]
    layer <- c(layer, list(
      encoding = eval_encoding(data, encoding[!which_selection])))

    if (has_length(encoding_sel)) {
      selection <- map(encoding_sel, eval_tidy, data = data)
      trues <- map(selection, function(x) x$true)
      falses <- map(selection, function(x) x$false)
      fields <- c(fields, trues, falses)
      condition <- eval_condition(data, selection, names(encoding_sel))
      layer$encoding <- c(layer$encoding, condition)
      selection <- vec_c(!!!map(selection, function(x) unclass(x$selection)),
        .name_spec = "{inner}")
      layer <- c(list(selection = selection), layer)
    }
  }

  # data needs updating
  fields <- vec_set_names(fields, map_chr(fields, as_field))
  data <- eval_encoding_mask(data, fields, names(encoding))
  if (is_data_inherit) {
    v$data$values <- data
  } else {
    layer <- c(list(data = list(values = data)), layer)
  }

  spec <- build_layer(v, add_layer(v$layer, layer))
  new_virgo(spec)
}

merge_encoding <- function(x) {
  x <- rev(x)
  names_x <- names(x)
  x <- rev(x[vec_match(vec_unique(names_x), names_x)])
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
  input_lgl <- map_lgl(dots, is_virgo_input)
  params <- vec_init(list(), n = sum(input_lgl))
  for (i in seq_along(params)) {
    if (is_virgo_input(dots[input_lgl][[i]])) {
      input <- dots[input_lgl][[i]]
      dots[input_lgl][[i]] <- list(expr = input$name)
      params[[i]] <- list(name = input$name, value = input %@% "init", 
        bind = unclass(input))
    }
  }
  if (!(has_name(dots, "tooltip"))) { # enable tooltip by default
    dots$tooltip <- TRUE
  }
  if (!has_name(dots, "clip")) {
    dots$clip <- TRUE
  }
  list(props = dots, params = params)
}

# use vega options name but in snake_case
mark_factory <- function(type = "point") {
  force(type)
  function(v, encoding = NULL, data = NULL, selection = NULL, ...) {
    marks <- mark_properties(...)
    v$params <- marks$params
    layer <- list(mark = list2(type = type, !!!marks$props))
    vega_layer(v, layer, encoding, data, selection)
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
  position <- arg_match(position, c("identity", "stack", "fill"))
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
  }
}

mark_area <- function(v, encoding = NULL, data = NULL, selection = NULL, ...,
  position = "stack") {
  marks <- mark_properties(...)
  v$params <- marks$params
  layer <- list(mark = list2(type = "area", !!!marks$props))
  v <- vega_layer(v, layer, encoding, data, selection)
  last <- nlayer(v)
  v$layer[[last]]$encoding$y$stack <- position_to_stack(position)
  v$layer[[last]]$encoding$y$scale$zero <- TRUE
  v
}

mark_bar <- function(v, encoding = NULL, data = NULL, selection = NULL, ...,
  position = "stack") {
  marks <- mark_properties(...)
  v$params <- marks$params
  layer <- list(mark = list2(type = "bar", !!!marks$props))
  v <- vega_layer(v, layer, encoding, data, selection)
  last <- nlayer(v)
  v$layer[[last]]$encoding$y$scale$zero <- TRUE
  v$layer[[last]]$encoding$y$stack <- position_to_stack(position)
  v
}

mark_errorbar <- function(v, encoding = NULL, data = NULL, selection = NULL,
  ...) {
  marks <- mark_properties(ticks = TRUE, ...)
  v$params <- marks$params
  layer <- list(mark = list2(type = "errorbar", !!!marks$props))
  vega_layer(v, layer, encoding, data, selection)
}

mark_histogram <- function(v, encoding = NULL, data = NULL, selection = NULL,
  ..., position = "stack", bin = TRUE) { # bin = list() opts
  v <- mark_bar(v, encoding, data, selection, ..., position = position)
  last <- nlayer(v)
  v$layer[[last]]$encoding$x$scale$padding <- 10
  x <- v$layer[[last]]$encoding$x
  y <- v$layer[[last]]$encoding$y
  v$layer[[last]]$encoding$x <- c(x, list(bin = bin))
  v$layer[[last]]$encoding$y <- c(y, aggregate = "count")
  v
}

mark_step <- function(v, encoding = NULL, data = NULL, selection = NULL, ...) {
  marks <- mark_properties(interpolate = "step-after", ...)
  v$params <- marks$params
  layer <- list(mark = list2(type = "line", !!!marks$props))
  vega_layer(v, layer, encoding, data, selection)
}

mark_density <- function(v, encoding = NULL, data = NULL, selection = NULL, ...,
  position = "identity", density = list()) {
  v <- mark_area(v, encoding, data, selection, ..., position = position)
  last <- nlayer(v)
  enc <- v$layer[[last]]$encoding
  density_field <- enc$x$field
  groupby <- as.list(unique(c(enc$color$field, enc$fill$field, enc$detail$field,
    enc$stroke$field)))
  dens <- list2(density = density_field, groupby = groupby, !!!density)
  trans <- vec_c(!!!v$layer[[last]]$transform)
  if (is.null(trans)) {
    v$layer[[last]]$transform <- list(dens)
  } else {
    v$layer[[last]]$transform <- list(trans, dens)
  }
  v$layer[[last]]$encoding$x$field <- "value"
  v$layer[[last]]$encoding$y <- c(enc$y, field = "density", type = "quantitative")
  v
}

mark_bin2d <- function(v, encoding = NULL, data = NULL, selection = NULL, ...,
  bin = list(x = TRUE, y = TRUE)) {
  # list(x = list(maxbins = 10))
  marks <- mark_properties(...)
  v$params <- marks$params
  layer <- list(mark = list2(type = "rect", !!!marks$props))
  v <- vega_layer(v, layer, encoding, data, selection)
  last <- nlayer(v)
  x <- v$layer[[last]]$encoding$x
  y <- v$layer[[last]]$encoding$y
  v$layer[[last]]$encoding$x <- c(x, list(bin = bin$x))
  v$layer[[last]]$encoding$y <- c(y, list(bin = bin$y))
  v
}

mark_streamgraph <- function(v, encoding = NULL, data = NULL, selection = NULL,
  ...) {
  marks <- mark_properties(...)
  v$params <- marks$params
  layer <- list(mark = list2(type = "area", !!!marks$props))
  v <- vega_layer(v, layer, encoding, data, selection)
  last <- nlayer(v)
  v$layer[[last]]$encoding$y$stack <- "center"
  # remove y axis as y values not important
  v$layer[[last]]$encoding$y <- c(v$layer[[last]]$encoding$y, list(axis = NULL))
  v
}

mark_smooth <- function(v, encoding = NULL, data = NULL, selection = NULL, ...,
  method = "lm", formula = y ~ x, bandwidth = 0.3) {
  marks <- mark_properties(...)
  v$params <- marks$params
  method <- arg_match(method, c("lm", "loess"))
  method <- if (method == "lm") "regression" else "loess"
  layer <- list(mark = list2(type = "line", !!!marks$props))
  v <- vega_layer(v, layer, encoding, data, selection)
  last <- nlayer(v)
  enc <- v$layer[[last]]$encoding
  groupby <- as.list(unique(c(enc$color$field, enc$fill$field, enc$detail$field,
    enc$stroke$field)))
  f <- interpret_formula(formula)
  smooth_fn <- list2(!!method := enc$y$field, on = enc$x$field,
    groupby = groupby, !!!f, bandwidth = bandwidth)
  trans <- vec_c(!!!v$layer[[last]]$transform)
  if (is.null(trans)) {
    v$layer[[last]]$transform <- list(smooth_fn)
  } else {
    v$layer[[last]]$transform <- list(trans, smooth_fn)
  }
  v
}

interpret_formula <- function(formula) {
  # TODO:
  # 1. abort if more than one calls in the specified formula
  # 2. no support for "pow" option, don't know how to distinguish pow and poly
  rhs <- f_rhs(formula)
  if (is_symbol(rhs)) {
    list(method = "linear")
  } else if (is_call(rhs, "log")) {
    list(method = "log")
  } else if (is_call(rhs, "exp")) {
    list(method = "exp")
  } else if (is_call(rhs, "^")) {
    order <- call_args(rhs)[[2]]
    if (order == 2) {
      list(method = "quad")
    } else {
      list(method = "poly", order = order)
    }
  }
}
