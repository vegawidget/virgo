enc <- function(x, y, ...) {
  enquos(x = x, y = y, ..., .ignore_empty = "all")
}

new_virgo_op <- function(x) {
  structure(x, class = c("virgo_aggregate", "virgo_op"))
}

is_virgo_op <- function(x) {
  inherits(x, "virgo_op")
}

vg_sum <- function(x) {
  stopifnot(!is_missing(x))
  new_virgo_op(list(aggregate = "sum", type = "quantitative"))
}

vg_mean <- function(x) {
  stopifnot(!is_missing(x))
  new_virgo_op(list(aggregate = "mean", type = "quantitative"))
}

vg_count <- function(x) { # "count" can take empty values
  new_virgo_op(list(aggregate = "count", type = "quantitative"))
}

eval_fun <- function(data, encoding) {
  spec <- eval_tidy(encoding, data = data)
  encoding_spec(spec, field = encoding)
}

eval_encoding <- function(data, encoding) {
  lst <- map(encoding, function(x) eval_fun(data, x))
  vec_set_names(lst, names(encoding))
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
      def_true <- encoding_spec(eval_true, field = cond_true)
    } else {
      def_true <- list(value = eval_true)
    }
    if (quo_is_symbol(cond_false) || quo_is_call(cond_false)) {
      def_false <- encoding_spec(eval_false, field = cond_false)
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
    as_field(call_args(quo)[[1]])
  } else if (quo_is_symbol(quo)) {
    as_name(quo)
  } else if (quo_is_call(quo)) {
    as_field(call_args(quo_get_expr(quo))[[1]])
  }
}

encoding_spec <- function(x, field) {
  UseMethod("encoding_spec")
}

encoding_spec.default <- function(x, field) {
  type <- data_type(x)
  res <- list(field = as_field(field), type = type)
  if (type == "quantitative") {
    rng <- range(x, na.rm = TRUE)
    width <- diff(rng)
    min_x <- min(x, na.rm = TRUE) - 0.05 * width
    max_x <- max(x, na.rm = TRUE) + 0.05 * width
    domain <- c(min_x, max_x)
    res <- list2(!!!res, scale = list(zero = FALSE, domain = domain))
  }
  res
  # list(field = as_field(field), type = data_type(x), scale = list(zero = FALSE))
}

encoding_spec.virgo_op <- function(x, field) {
  list2(field = as_field(field), !!!unclass(x), scale = list(zero = FALSE))
}
