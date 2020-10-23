enc <- function(x, y, ...) {
  enquos(x = x, y = y, ..., .ignore_empty = "all")
}

simple_select <- function(x) {
  x <- enexpr(x)
  if (is_call(x)) {
    stopifnot(call_name(x) == "c")
    args <- call_args(x)
    map_chr(args, as_string)
  } else if (is.null(x)) {
    NULL
  } else {
    as_string(x)
  }
}

eval_enc <- function(data, encoding, encoding_name) {
  spec <- eval_tidy(encoding, data = data)
  encoding_spec(spec, field = encoding, encoding_name = encoding_name)
}

eval_encoding <- function(data, encoding) {
  encoding <- vec_set_names(encoding, standardise_names(names(encoding)))
  map2(encoding, names(encoding), function(x, y) eval_enc(data, x, y))
}

eval_condition <- function(data, selection) {
  selection_cp <- selection
  n_sel <- length(selection_cp)
  res <- vec_init(list(), n = n_sel)
  for (i in seq_len(n_sel)) {
    selection <- selection_cp[[i]]
    encoding <- selection$encoding
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
    res[[i]] <- list2(!!encoding := list2(
      condition = list2(
        selection = selection_composition(selection),
        !!!def_true),
      !!!def_false
    ))
  }
  vec_c(!!!res)
}

as_field <- function(quo) {
  if (is_symbol(quo)) {
    as_string(quo)
  } else if (!is_quosure(quo) && is_call(quo)) {
    args <- call_args(quo)
    if (is_empty(args)) {
      ""
    } else {
      as_field(args[[1]])
    }
  } else if (quo_is_symbol(quo)) {
    as_name(quo)
  } else if (quo_is_call(quo)) {
    args <- call_args(quo_get_expr(quo))
    if (is_empty(args)) {
      ""
    } else {
      as_field(args[[1]])
    }
  } else if (quo_is_null(quo)) {
    NULL
  } else {
    ""
  }
}

encoding_spec <- function(x, field, ...) {
  UseMethod("encoding_spec")
}

encoding_spec.default <- function(x, field, ...) {
  type <- data_type(x)
  list2(field = as_field(field), !!!type)
}

encoding_spec.numeric <- function(x, field, encoding_name, ...) {
  type <- data_type(x)
  res <- list2(field = as_field(field), !!!type)
  if (any(vec_in(c("color", "fill"), encoding_name))) { return(res) }
  rng <- range(x, na.rm = TRUE)
  width <- diff(rng)
  min_x <- min(x, na.rm = TRUE) - 0.05 * width
  max_x <- max(x, na.rm = TRUE) + 0.05 * width
  domain <- c(min_x, max_x)
  list2(!!!res, scale = list(domain = domain))
}

encoding_spec.factor <- function(x, field, encoding_name, ...) {
  type <- data_type(x)
  res <- list2(field = as_field(field), !!!type)
  ncat <- length(vec_unique(x))
  if (any(vec_in(c("color", "fill"), encoding_name))) {
    res <- list2(!!!res, scale = list(range = scales::hue_pal()(ncat)))
  }
  res
}

encoding_spec.character <- encoding_spec.factor

encoding_spec.virgo_aggregate <- function(x, field, ...) {
  list2(
    field = as_field(field), aggregate = x %@% "aggregate",
    type = "quantitative", scale = list(zero = FALSE))
}

encoding_spec.virgo_timeunit <- function(x, field, ...) {
  list2(field = as_field(field), 
    timeUnit = x %@% "timeUnit", step = x %@% "step", utc = x %@% "utc",
    type = "temporal")
}

virgo_op_env <- function() {
  ops <- virgo_op()
  fns <- map(ops, function(op) function(x) {
    if (is_virgo_op(x)) { unclass(x) } else { x }
  })
  new_environment(vec_set_names(fns, ops))
}

new_virgo_mask <- function(data, env = virgo_op_env()) {
  bottom <- as_environment(data, parent = env)
  new_data_mask(bottom, top = env)
}

eval_virgo_mask <- function(data, quo) {
  names <- names(quo)
  data_mask <- new_virgo_mask(data)
  for (i in names) {
    data[[i]] <- eval_tidy(quo[[i]], data = data_mask)
  }
  data
}
