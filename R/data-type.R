data_type <- function(x) {
  UseMethod("data_type")
}

data_type.numeric <- function(x) {
  "quantitative"
}

data_type.factor <- function(x) {
  "nominal"
}

data_type.character <- data_type.factor

data_type.POSIXt <- function(x) {
  "temporal"
}

data_type.Date <- data_type.POSIXt
