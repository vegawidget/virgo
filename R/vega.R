#' @import rlang vctrs vegawidget

new_virgo <- function(spec) {
  structure(spec, class = "virgo")
}

vega <- function(data = NULL, encoding = enc(), width = 300, height = 300) {
  spec <- list(
    data = list(values = data), encoding = encoding,
    width = width, height = height)
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
  if (has_name(spec, "facet")) {
    spec$spec$layer <- spec$layer
    spec$layer <- NULL
  }
  as_vegaspec(c(spec_header, spec))
}

#' @export
print.virgo <- function(x, ...) {
  print(as_vegaspec(x), ...)
  invisible(x)
}
