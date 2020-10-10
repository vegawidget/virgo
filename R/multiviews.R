# No facet_wrap() since vega `layer` doesn't handle `facet` encoding

facet_views <- function(v, row = NULL, column = NULL) {
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

hconcat <- function(...) {
  lst <- map(list2(...), function(x) { x$encoding <- NULL; unclass(x) })
  spec <- list(hconcat = list2(!!!lst))
  new_virgo(spec)
}

vconcat <- function(...) {
  lst <- map(list2(...), function(x) { x$encoding <- NULL; unclass(x) })
  spec <- list(vconcat = list2(!!!lst))
  new_virgo(spec)
}
