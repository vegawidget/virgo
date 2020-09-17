data_type <- function(x) {
  UseMethod("data_type")
}

data_type.numeric <- function(x) {
  "quantitative"
}

data_type.factor <- function(x) {
  "ordinal"
}

data_type.character <- function(x) {
  "nominal"
}

data_type.POSIXt <- function(x) {
  "temporal"
}

data_type.Date <- date_type.POSIXt
