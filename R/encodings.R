enc <- function(x, y, ...) {
  enquos(x = x, y = y, ..., .ignore_empty = "all")
}

# ToDo: fun(var)
# 0: bare variable
# 1. factor() -> eval in R, and make a name `field: factor(cyl)`
# 2. mean() -> `aggregate: mean, field: mean(cyl)`, 
# 3. mean2() -> eval in R, `field: mean2(cyl)`, 
# Algortihm:
#  check whether quosuore is  a call?
# if not -> return and use whatever you have already
# if yes -> evaluate quousure inside a new envirnonment where the functions are 
# mapped to the vega lite aggregate transforms mean <- function(x) list("aggregate" = "mean", field = as_name(x))
# 3. eval_tidy in the data context, data$`fun(col)` assigned to result