#' Map data variables to visual encodings
#'
#' @param x,y,... A set of name-value pairs to describe the mappings of data
#' variables to visual encodings in `vega()` and individual mark layers `mark_*()`.
#' Use `NULL` to disable a layer encoding to inherit from its parent encodings.
#'
#' @return A list of quosures or constants.
#' @export
#' @examples
#' enc(x = mpg, y = wt)
#' enc(colour = cyl)
#' enc(color = cyl)
#' enc(x = NULL)
enc <- function(x, y, ...) {
  encoding <- enquos(x = x, y = y, ..., .ignore_empty = "all")
  s_names <- standardise_encodings(names(encoding))
  vec_set_names(encoding, s_names)
}

simple_select <- function(x) {
  x <- enexpr(x)
  if (is_call(x, "c")) {
    args <- call_args(x)
    map_chr(args, as_string)
  } else if (is.null(x)) {
    NULL
  } else {
    as_string(x)
  }
}

eval_enc <- function(data, encoding, encoding_name) {
  if (encoding_name == "tooltip") { # column names only, no functions
    cols <- names(eval_select(encoding, data))
    map(cols, function(x) encoding_spec(data[[x]], sym(x), "tooltip"))
  } else {
    spec <- eval_tidy(encoding, data = data)
    encoding_spec(spec, field = encoding, encoding_name = encoding_name,
      data = data)
  }
}

eval_encoding <- function(data, encoding) {
  map2(encoding, names(encoding), function(x, y) eval_enc(data, x, y))
}

eval_condition <- function(data, selection, encoding) {
  selection_cp <- selection
  n_sel <- length(selection_cp)
  res <- vec_init(list(), n = n_sel)
  for (i in seq_len(n_sel)) {
    selection <- selection_cp[[i]]
    cond_true <- selection$true
    cond_false <- selection$false
    selection <- selection$selection
    eval_true <- eval_tidy(cond_true, data = data)
    eval_false <- eval_tidy(cond_false, data = data)
    if (quo_is_symbol(cond_true) || quo_is_call(cond_true)) {
      def_true <- encoding_spec(eval_true, cond_true, encoding)
    } else {
      def_true <- list(value = eval_true)
    }
    if (quo_is_symbol(cond_false) || quo_is_call(cond_false)) {
      def_false <- encoding_spec(eval_false, cond_false, encoding)
    } else {
      def_false <- list(value = eval_false)
    }
    res[[i]] <- list2(!!encoding[i] := list2(
      condition = list2(
        selection = selection_composition(selection),
        !!!def_true),
      !!!def_false
    ))
  }
  vec_c(!!!res)
}

as_field_rhs <- function(quo) {
  if (is_quosure(quo)) {
    if (quo_is_null(quo)) {
      NULL
    } else if (quo_is_call(quo)) {
      fn <- call_name(quo)
      if (vec_in(fn, "vg_count")) {
        ""
      } else if  (vec_in(fn, "ac")){
        ""
      } else if (vec_in(fn, virgo_op())) {
        as_label(call_args(quo)[[1]])
      } else {
        as_label(quo)
      }
    } else {
      as_label(quo)
    }
  } else {
    as_label(quo)
  }

}

as_field <- function(quo) {
  square_brackets(as_field_rhs(quo))
}

encoding_spec <- function(x, field, ...) {
  UseMethod("encoding_spec")
}

encoding_spec.default <- function(x, field, ...) {
  type <- data_type(x)
  list2(field = as_field(field), !!!type)
}

encoding_spec.Date <- function(x, field, encoding_name, ...) {
  type <- data_type(x)
  res <- list2(field = as_field(field), !!!type)
  if (any(vec_in(c("x", "y"), encoding_name))) {
    res <- list2(!!!res, scale = list(padding = 10))
  }
  res
}

encoding_spec.numeric <- function(x, field, encoding_name, ...) {
  type <- data_type(x)
  res <- list2(field = as_field(field), !!!type)
  if (any(vec_in(c("x", "x2", "y", "y2"), encoding_name))) {
    res <- list2(!!!res, scale = list(domain = expand_domain(x)),
      axis = list(tickCount = 5))
  }
  res
}

encoding_spec.factor <- function(x, field, encoding_name, ...) {
  type <- data_type(x)
  res <- list2(field = as_field(field), !!!type)
  ncat <- length(vec_unique(x))
  if (any(vec_in(c("color", "fill"), encoding_name))) {
    res <- list2(!!!res, scale = list(range = scales::hue_pal()(ncat)))
  }
  res$scale$paddingOuter <- 0.2
  res
}

encoding_spec.character <- encoding_spec.factor

encoding_spec.logical <- encoding_spec.factor

encoding_spec.virgo_aggregate <- function(x, field, encoding_name, ...) {
  data <- dots_list(...)$data
  aggregate <- x$aggregate
  type <- data_type(data[[as_field(field)]])
  if (vec_in(aggregate, c("argmin", "argmax"))) {
    arg_field <- as_string(call_args(field)[[2]])
    res <- list2(
      field = as_field(field), aggregate = list2(!!aggregate := arg_field),
      !!!type)
  } else {
    res <- list2(field = as_field(field), aggregate = aggregate, !!!type)
  }
  if (any(vec_in(c("x", "x2", "y", "y2"), encoding_name))) {
    res <- list2(!!!res, scale = list(zero = FALSE, padding = 10))
  }
  res
}

encoding_spec.virgo_timeunit <- function(x, field, encoding_name, ...) {
  res <- list2(field = as_field(field), timeUnit = unclass(x), type = "temporal")
  if (any(vec_in(c("x", "x2", "y", "y2"), encoding_name))) {
    res <- list2(!!!res, scale = list(padding = 10))
  }
  res
}

encoding_spec.virgo_combinator <- function(x, field, encoding_name, ...) {
  data <- dots_list(...)$data
  cols <- x(data)
  encoders <- map(names(cols),
               function(x) encoding_spec(data[[x]], sym(x), encoding_name))

  types <- reduce(map_chr(encoders, function(x) x$type), union)

  if (length(types) != 1L) {
    abort("Repeated fields must evaluate to same type")
  }

  if (!encoding_name %in% c("x", "y")) {
    abort("Repeated fields must be encoded to x or y")
  }
  where <- c(x = "column", y = "row")
  list2(
    field = list("repeat" = unname(where[encoding_name]), type = types)
  )
}

encoding_spec.virgo_bin <- function(x, field, encoding_name, ...) {
  res <- list2(field = as_field(field), bin = unclass(x))
  if (any(vec_in(c("x", "x2", "y", "y2"), encoding_name))) {
    res <- list2(!!!res, scale = list(padding = 10))
  }
  res
}

encoding_spec.virgo_window <- function(x, field, ...) {
  abort("`encoding` specs don't know how to handle `vg_window()`.")
}

ac <- function(...) {
  selector <- function(.data) {
    tidyselect::eval_select(expr(c(...)), .data)
  }

  structure(.Data = selector, class = c("virgo_combinator", "function"))
}

virgo_encoding_env <- function() {
  ops <- c(virgo_op(), "encode_if")
  fns <- map(ops, function(op) function(x, ...) {
    if (is_missing(x) || is_virgo_selection(x)) { # vg_count() with missing arg
      NULL
    }
    else {
      x
    }
  })
  new_environment(vec_set_names(fns, ops))
}

new_virgo_mask <- function(data, env = virgo_encoding_env()) {
  bottom <- as_environment(data, parent = env)
  new_data_mask(bottom, top = env)
}

eval_encoding_mask <- function(data, quo, encoding_name) {
  names <- names(quo)
  data_mask <- new_virgo_mask(data)
  for (i in seq_along(names)) {
    if (quo_is_call(quo[[i]], "ac")) {
      next
    }
    data[[names[i]]] <- eval_tidy(quo[[i]], data = data_mask)
  }
  data
}


eval_repeater <- function(data, quo, encoding_name) {
  res <- list()
  names <- names(quo)
  data_mask <- new_virgo_mask(data)
  where <- c("x" = "column", "y" = "row")
  for (i in seq_along(names)) {
    if (quo_is_call(quo[[i]], "ac")) {

      .selector <- eval_tidy(quo[[i]], data = data_mask)
      cols <- .selector(data)
      res[[unname(where[encoding_name[[i]]])]] <- names(cols)
    }
  }
  res
}
