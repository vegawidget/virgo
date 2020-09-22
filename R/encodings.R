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
    update_col <- eval_tidy(enc, data = data)
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
    field <- as_field(enc)
    type <- data_type(eval_tidy(enc, data = data))
    lst[[i]] <- list(field = field, type = type)
  }
  lst
}

as_field <- function(quo) {
  if (quo_is_symbol(quo)) return(as_name(quo))
  as_label(quo)
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
agg_factory <- function(op) {
  function(x) {
    list("aggregate" = op, field = x)
  }
}


encode_ops <- function() {
  # aggregate transforms go here
  ops <- c("count", "distinct", "sum", "mean")
  bare_fns <- vec_init(list(), n = length(ops))
  masked_fns <- vec_set_names(bare_fns, ops)
  for (op in ops) {
    masked_fns[[op]] <- agg_factory(op)
  }

  masked_fns
}
