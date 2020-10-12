data_type <- function(x) {
  UseMethod("data_type")
}

data_type.numeric <- function(x) {
  list(type = "quantitative")
}

data_type.character <- function(x) {
  list(type = "nominal")
}

data_type.logical <- data_type.character

data_type.factor <- function(x) {
  list(type = "nominal", sort = levels(x))
}

data_type.ordered <- function(x) {
  list(type = "ordinal", sort = levels(x))
}

data_type.POSIXt <- function(x) {
  list(type = "temporal")
}

data_type.Date <- data_type.POSIXt

data_type.NULL <- function(x) {
  NULL
}
