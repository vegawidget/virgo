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
  new_virgo_op(list(aggregate = "sum", type = "quantitative"))
}

vg_mean <- function(x) {
  new_virgo_op(list(aggregate = "mean", type = "quantitative"))
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
  encoding <- selection$encoding
  cond_true <- selection$true
  cond_false <- selection$false
  selection <- selection$selection
  if (quo_is_symbol(cond_true) || quo_is_call(cond_true)) {
    def_true <- list(
      field = as_field(cond_true),
      type = data_type(eval_tidy(cond_true, data = data)))
  } else {
    def_true <- list(value = eval_tidy(cond_true))
  }
  if (quo_is_symbol(cond_false) || quo_is_call(cond_false)) {
    def_false <- list(
      field = as_field(cond_false),
      type = data_type(eval_tidy(cond_false, data = data)))
  } else {
    def_false <- list(value = eval_tidy(cond_false))
  }
  list2(!!encoding := list2(
    condition = list2(selection = selection_name(selection), !!!def_true),
    !!!def_false
  ))
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

encoding_spec <- function(x, field) { UseMethod("encoding_spec") }

encoding_spec.default <- function(x, field) {
  list(field = as_field(field), type = data_type(x), scale = list(zero = FALSE))
}

encoding_spec.virgo_op <- function(x, field) {
  list2(field = as_field(field), !!!unclass(x))
}
