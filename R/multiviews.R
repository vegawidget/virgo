#' Facet data views by rows and columns
#'
#' @param v A `vega()` object.
#' @param row,column A set of data variables to define facetted views on the
#' rows and columns grid.
#'
#' @export
facet_views <- function(v, row = NULL, column = NULL) {
  # No facet_wrap() since vega `layer` doesn't handle `facet` encoding
  abort_if_not_virgo(v)
  v$facet <- list()
  row <- enexpr(row)
  column <- enexpr(column)
  if (!is.null(row)) {
    v$facet <- c(v$facet, list(row = list(field = simple_select(!!row))))
  }
  if (!is.null(column)) {
    v$facet <- c(v$facet, list(column = list(field = simple_select(!!column))))
  }
  v
}

#' Concatenate views
#'
#' `hconcat()` for horizontal concatenation, and `vconcat()` for vertical
#' concatenation
#'
#' @param ... A list of `vega()` objects.
#'
#' @rdname concat
#' @export
hconcat <- function(...) {
  lst <- list2(...)
  map(lst, abort_if_not_virgo)
  lst <- map(lst, function(x) { x$encoding <- NULL; as_vegaspec(x) })
  spec <- list(hconcat = list2(!!!lst))
  new_virgo(spec)
}

#' @rdname concat
#' @export
vconcat <- function(...) {
  lst <- list2(...)
  map(lst, abort_if_not_virgo)
  lst <- map(lst, function(x) { x$encoding <- NULL; as_vegaspec(x) })
  spec <- list(vconcat = list2(!!!lst))
  new_virgo(spec)
}

#' Resolve scale and guide for layered and multi-view displays
#'
#' @inheritParams facet_views
#' @param scale A named list of every channel to define either "shared" or 
#' "independent".
#' @param axis A named list of positional channels like `x` and `y`.
#' @param legend A named list of non-positional channels, such as `color`/`colour`,
#' `opacity`, `shape`, and `size`.
#'
#' @export
resolve_views <- function(v, scale = list(), axis = list(), legend = list()) {
  abort_if_not_virgo(v)
  scale <- vec_set_names(scale, standardise_names(names(scale)))
  axis <- vec_set_names(axis, standardise_names(names(axis)))
  legend <- vec_set_names(legend, standardise_names(names(legend)))
  v$resolve <- list(scale = scale, axis = axis, legend = legend)
  v
}
