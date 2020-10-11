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
  spec_header <- list(`$schema` = vega_schema())
  if (!has_name(spec, "config")) {
    spec <- config_ggplot(spec)
  }
  spec <- unclass(spec)
  if (is.null(spec$data$values)) {
    spec$data <- NULL
  }
  # remove top-level encoding, since it already applies to each layer
  spec$encoding <- NULL
  # unify default scale domains
  layer <- spec$layer
  xs <- map(layer, function(x) 
    c(x$encoding$x$scale$domain, x$encoding$x2$scale$domain))
  ys <- map(layer, function(x) 
    c(x$encoding$y$scale$domain, x$encoding$y2$scale$domain))
  xvec <- vec_c(!!!xs)
  yvec <- vec_c(!!!ys)
  xrng <- yrng <- NULL
  if (!is.null(xvec) && !has_name(xvec, "selection")) {
    xrng <- range(xvec)
  }
  if (!is.null(yvec) && !has_name(yvec, "selection")) {
    yrng <- range(yvec)
  }
  for (i in seq_along(layer)) {
    mark_type <- layer[[i]]$mark$type
    if (mark_type == "bar") {
      xrng <- NULL
    }
    if (!is.null(layer[[i]]$encoding$x$scale$domain)) {
      spec$layer[[i]]$encoding$x$scale$domain <- xrng
    }
    if (!is.null(layer[[i]]$encoding$y$scale$domain)) {
      spec$layer[[i]]$encoding$y$scale$domain <- yrng
    }
  }
  # facet is used
  if (has_name(spec, "facet")) {
    spec$layer <- map(layer, function(x) { x$data <- NULL; x })
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
