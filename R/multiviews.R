simple_select <- function(x) {
  x <- enexpr(x)
  if (is_call(x)) {
    stopifnot(call_name(x) == "c")
    args <- call_args(x)
    map_chr(args, as_string)
  } else if (is.null(x)) {
    NULL
  } else {
    as_string(x)
  }
}

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
  lst <- map(list2(...), unclass)
  spec <- list(hconcat = list2(!!!lst))
  new_virgo(spec)
}

vconcat <- function(...) {
  lst <- map(list2(...), unclass)
  spec <- list(vconcat = list2(!!!lst))
  new_virgo(spec)
}
