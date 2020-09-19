enc <- function(x, y, ...) {
  enquos(x = x, y = y, ..., .ignore_empty = "all")
}

eval_encoding <- function(data, encoding) {
  x <- encoding$x
  y <- encoding$y
  color <- encoding$color
  x_field <- as_name(x)
  y_field <- as_name(y)
  color_field <- as_name(color)
  x_type <- data_type(eval_tidy(x, data = data))
  y_type <- data_type(eval_tidy(y, data = data))
  color_type <- data_type(eval_tidy(color, data = data))
  list(
    x = list(field = x_field, type = x_type),
    y = list(field = y_field, type = y_type),
    color = list(field = color_field, type = color_type)
  )
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
