enc <- function(x, y, ...) {
  enquos(x = x, y = y, ..., .ignore_empty = "all")
}

eval_encoding <- function(data, encoding) {
  channels <- names(encoding)
  bare_lst <- vec_init(list(), n = length(encoding))
  lst <- vec_set_names(bare_lst, channels)
  for (i in channels) {
    enc <- encoding[[i]]
    field <- as_name(enc)
    type <- data_type(eval_tidy(enc, data = data))
    lst[[i]] <- list(field = field, type = type)
  }
  lst
}

# ToDo: fun(var)
# 0: bare variable
# 1. factor() -> eval in R, and make a name `field: factor(cyl)`
# 2. mean() -> `aggregate: mean, field: mean(cyl)`,
# 3. mean2() -> eval in R, `field: mean2(cyl)`,
# 4. ifelse(selection, factor(cyl), "grey")
# Algortihm:
#  check whether quosuore is  a call?
# if not -> return and use whatever you have already
# if yes -> evaluate quousure inside a new envirnonment where the functions are
# mapped to the vega lite aggregate transforms mean <- function(x) list("aggregate" = "mean", field = as_name(x))
# 3. eval_tidy in the data context, data$`fun(col)` assigned to result
