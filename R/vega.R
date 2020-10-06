#' @import rlang vctrs vegawidget

new_virgo <- function(spec) {
  structure(spec, class = "virgo")
}

vega <- function(data = NULL, encoding = enc()) {
  spec <- list(data = list(values = data), encoding = encoding)
  new_virgo(spec)
}

hconcat <- function(...) {
  lst <- map(list(...), unclass)
  spec <- list(hconcat = list2(!!!lst))
  new_virgo(spec)
}

#' @export
as_vegaspec.virgo <- function(spec, ...) {
  spec_header <- list(`$schema` = vega_schema(), config = config_ggplot())
  spec <- unclass(spec)
  if (is.null(spec$data$values)) {
    spec$data <- NULL
  }
  # remove top-level encoding, since it already applies to each layer
  spec$encoding <- NULL
  # TODO: unify default scales when no of layers > 1
  as_vegaspec(c(spec_header, spec))
}

#' @export
print.virgo <- function(x, ...) {
  print(as_vegaspec(x), ...)
  invisible(x)
}
