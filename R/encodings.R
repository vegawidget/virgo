enc <- function(x, y, ...) {
  enquos(x = x, y = y, ..., .ignore_empty = "all")
}

eval_values <- function(data, encoding) {
  channels <- names(encoding)
  # probably could just do this with a mutate() instead
  for (i in channels) {
    enc <- encoding[[i]]
    if (quo_is_symbol(enc))
      next
    update_col <- eval_tidy(enc, data = new_vega_mask(data))
    if (inherits(update_col, "vega_op"))
      next
    update_col_field <- as_label(enc)
    data <- vec_cbind(data, !!update_col_field := update_col)
  }
  data
}

eval_encoding <- function(data, encoding) {
  channels <- names(encoding)
  bare_lst <- vec_init(list(), n = length(encoding))
  lst <- vec_set_names(bare_lst, channels)
  for (i in channels) {
    enc <- encoding[[i]]
    res <- eval_tidy(enc, data = new_vega_mask(data))
    field <- as_field(enc)
    lst[[i]] <- encoding_spec(res, field = field)
  }
  lst
}

eval_condition <- function(data, selection) {
  encoding <- selection$encoding
  cond_true <- selection$true
  cond_false <- selection$false
  selection <- selection$selection
  if (quo_is_symbol(cond_true)) {
    def_true <- list(
      field = as_field(cond_true),
      type = data_type(eval_tidy(cond_true, data = new_vega_mask(data))))
  } else {
    def_true <- list(value = eval_tidy(cond_true))
  }
  if (quo_is_symbol(cond_false)) {
    def_false <- list(
      field = as_field(cond_false),
      type = data_type(eval_tidy(cond_false, data = new_vega_mask(data))))
  } else {
    def_false <- list(value = eval_tidy(cond_false))
  }
  list2(!!encoding := list2(
    condition = list2(selection = selection %@% "name", !!!def_true),
    !!!def_false
  ))
}

as_field <- function(quo) {
  if (quo_is_symbol(quo)) return(as_name(quo))
  as_label(quo)
}

encoding_spec <- function(x, ...) { UseMethod("encoding_spec") }

encoding_spec.default <- function(x, ...) {
  list(..., type = data_type(x), scale = list(zero = FALSE))
}

encoding_spec.vega_op <- function(x, ...) {
  unclass(x)
}

# ToDo: fun(var)
# 0: bare variable
# 1. factor() -> eval in R, and make a name `field: factor(cyl)`
# 2. mean() -> `aggregate: mean, field: mean(cyl)`,
# 3. mean2() -> eval in R, `field: mean2(cyl)`,
# 4. ifelse(selection, factor(cyl), "grey")
# Algorithm:
# For 1. or 3. need to actually update values inside the data -> see eval_values
# if not -> return and use whatever you have already
# if yes -> evaluate quousure inside a new envirnonment where the functions are
# mapped to the vega lite aggregate transforms mean <- function(x) list("aggregate" = "mean", field = as_name(x))
# 3. eval_tidy in the data context, data$`fun(col)` assigned to result

new_agg_op <- function(op, x) {
  stopifnot(op %in% valid_ops())
  cls <- c("aggregate", "vega_op")

  structure(list(aggregate = op, field = x), class = cls)
}

valid_ops <- function() {
  # aggregate transforms go here
  c("count", "distinct", "sum", "mean")
}

gen_agg_factory <- function() {
  ops <- valid_ops()
  env <- env()
  fns <- map(ops, function(op) function(x) {
    x <- as_name(enexpr(x))

    new_agg_op(op, x)
  })
  names(fns) <- ops
  fns
}

new_vega_mask <- function(data, factory = gen_agg_factory()) {
  top <- new_environment(factory)
  bottom <- as_environment(data, parent = top)
  new_data_mask(bottom, top = top)
}
