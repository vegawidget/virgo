#' @import rlang tidyselect vctrs
#' @importFrom vegawidget as_vegaspec vega_embed vega_schema vegawidget `%>%`
#' @export `%>%`

new_virgo <- function(spec) {
  structure(spec, class = "virgo")
}

#' Create a new vega visualisation
#'
#' @param data A data frame.
#' @param encoding A list of aethetic encodings via [`enc()`].
#' @param width,height Data plotting width and height.
#'
#' @export
vega <- function(data = NULL, encoding = enc(), width = 300, height = 300) {
  spec <- list(
    data = list(values = data), encoding = encoding,
    width = width, height = height)
  new_virgo(spec)
}

#' @export
as.list.virgo <- function(x, ...) {
  unclass(as_vegaspec(x, ...))
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
  # remove top-level encoding & transform, since it already applies to each layer
  spec$encoding <- spec$transform <- NULL
  # unify default scale domains
  layer <- spec$layer
  xs <- map(layer, function(x)
    c(x$encoding$x$scale$domain, x$encoding$x2$scale$domain))
  ys <- map(layer, function(x)
    c(x$encoding$y$scale$domain, x$encoding$y2$scale$domain))
  xrng <- vec_c(!!!xs)
  yrng <- vec_c(!!!ys)
  if (!is.null(xrng) && !has_name(xrng, "selection") && !is_bare_list(xrng)) {
    xrng <- range(xrng)
  }
  if (!is.null(yrng) && !has_name(yrng, "selection") && !is_bare_list(yrng)) {
    yrng <- range(yrng)
  }
  for (i in seq_along(layer)) {
    if (!is.null(layer[[i]]$encoding$x$scale$domain)) {
      spec$layer[[i]]$encoding$x$scale$domain <- xrng
    }
    if (!is.null(layer[[i]]$encoding$y$scale$domain)) {
      spec$layer[[i]]$encoding$y$scale$domain <- yrng
    }
  }
  spec$layer <- selection_union(spec$layer)
  # facet is used
  if (has_name(spec, "facet")) {
    data <- spec$data$values
    rowvars <- spec$facet$row$field
    colvars <- spec$facet$col$field
    nrows <- vec_unique_count(data[, rowvars, drop = FALSE])
    ncols <- vec_unique_count(data[, colvars, drop = FALSE])
    spec$layer <- map(layer, function(x) { x$data <- NULL; x })
    spec$spec$layer <- spec$layer
    spec$spec$width <- spec$width / ncols
    spec$spec$height <- spec$height / nrows
    spec$layer <- NULL
  }
  as_vegaspec(c(spec_header, spec))
}

#' @export
print.virgo <- function(x, renderer = "canvas", ...) {
  renderer <- arg_match(renderer, c("canvas", "svg"))
  print(vegawidget(as_vegaspec(x),
    embed = vega_embed(renderer = renderer, actions = FALSE)), ...)
  invisible(x)
}


#' @export
format.virgo <- function(x, ...) {
  x <- as_vegaspec(x)
  format(x, ...)
}

#' @inheritParams vegawidget::knit_print.vegaspec
#' @rdname knit_print.vegaspec
#' @export
knit_print.virgo <- function(spec, ..., options = NULL) {
  spec <- as_vegaspec(spec)
  knitr::knit_print(spec, ..., options = options)
}

# NOTE: leave all styling properties to `config()`
entitle <- function(v, title = NULL, subtitle = NULL, description = NULL) {
  abort_if_not_virgo(v)
  v$title <- list(text = title, subtitle = subtitle)
  v$description <- description
  v
}

is_virgo <- function(v) {
  inherits(v, "virgo")
}

abort_if_not_virgo <- function(v) {
  if (!is_virgo(v)) {
    abort("Must be a `vega()` object.")
  }
}
