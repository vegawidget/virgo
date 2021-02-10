new_virgo_op <- function(x, ..., class) {
  structure(x, ..., class = c(class, "virgo_op"))
}

is_virgo_op <- function(x) {
  inherits(x, "virgo_op")
}

virgo_op <- function() {
  c("vg_sum", "vg_mean", "vg_count", "vg_distinct", "vg_median", "vg_min",
    "vg_max", "vg_argmin", "vg_argmax", "vg_bin",
    "vg_window_mean", "vg_window_sum", "vg_window_rank", "vg_window_count",
    "vg_cumsum", "vg_cummean", "vg_lead", "vg_lag", "vg_ntile", "vg_row_number",
    "vg_rank", "vg_dense_rank", "vg_percent_rank", "vg_cume_dist",
    "vg_year", "vg_quarter", "vg_month", "vg_yearmonth", "vg_date", "vg_week",
    "vg_day", "vg_dayofyear", "vg_hours", "vg_minutes", "vg_seconds",
    "vg_milliseconds")
}

virgo_aggregate_factory <- function(aggregate) {
  function(x) {
    new_virgo_op(list(aggregate = aggregate), class = "virgo_aggregate")
  }
}

#' @export
print.virgo_aggregate <- function(x, ...) {
  cat(fmt_bullets(x), sep = "\n")
  invisible(x)
}

#' Interactive aggregation operations
#'
#' @param x A data variable, used in conjunction with `enc()` or dplyr verbs.
#' `vg_count()` can accept an empty input.
#' @param y A data variable that maxmises/minimises `x` in `vg_argmin()` and
#' `vg_argmax()`.
#'
#' @rdname vg-aggregate
#' @export
vg_sum <- virgo_aggregate_factory("sum")

#' @rdname vg-aggregate
#' @export
vg_min <- virgo_aggregate_factory("min")

#' @rdname vg-aggregate
#' @export
vg_max <- virgo_aggregate_factory("max")

#' @rdname vg-aggregate
#' @export
vg_mean <- virgo_aggregate_factory("mean")

#' @rdname vg-aggregate
#' @export
vg_median <- virgo_aggregate_factory("median")

#' @rdname vg-aggregate
#' @export
vg_count <- virgo_aggregate_factory("count")

#' @rdname vg-aggregate
#' @export
vg_distinct <- virgo_aggregate_factory("distinct")

#' @rdname vg-aggregate
#' @export
vg_argmin <- function(x, y) {
  new_virgo_op(list(y = y, aggregate = "argmin"), class = "virgo_aggregate")
}

#' @rdname vg-aggregate
#' @export
vg_argmax <- function(x, y) {
  new_virgo_op(list(y = y, aggregate = "argmax"), class = "virgo_aggregate")
}

virgo_timeunit_factory <- function(unit) {
  force(unit)
  function(x, step = 1, utc = FALSE) {
    stopifnot(!is_missing(x))
    new_virgo_op(list(unit = unit, step = step, utc = utc),
      class = "virgo_timeunit")
  }
}

#' @export
print.virgo_timeunit <- print.virgo_aggregate

#' Interactive time unit operations
#'
#' @inheritParams vg_sum
#' @param step An integer to define the number of time steps.
#' @param utc If `TRUE`, parse data in UTC time, otherwise in local time.
#'
#' @rdname vg-timeunit
#' @export
vg_year <- virgo_timeunit_factory("year")

#' @rdname vg-timeunit
#' @export
vg_quarter <- virgo_timeunit_factory("quarter")

#' @rdname vg-timeunit
#' @export
vg_month <- virgo_timeunit_factory("month")

#' @rdname vg-timeunit
#' @export
vg_yearmonth <- virgo_timeunit_factory("yearmonth")

#' @rdname vg-timeunit
#' @export
vg_date <- virgo_timeunit_factory("date")

#' @rdname vg-timeunit
#' @export
vg_week <- virgo_timeunit_factory("week")

#' @rdname vg-timeunit
#' @export
vg_day <- virgo_timeunit_factory("day")

#' @rdname vg-timeunit
#' @export
vg_dayofyear <- virgo_timeunit_factory("dayofyear")

#' @rdname vg-timeunit
#' @export
vg_hours <- virgo_timeunit_factory("hours")

#' @rdname vg-timeunit
#' @export
vg_minutes <- virgo_timeunit_factory("minutes")

#' @rdname vg-timeunit
#' @export
vg_seconds <- virgo_timeunit_factory("seconds")

#' @rdname vg-timeunit
#' @export
vg_milliseconds <- virgo_timeunit_factory("milliseconds")

virgo_window_factory <- function(op) {
  force(op)
  function(x, frame = list(NULL, 0), sort = NULL) {
    sort <- simple_sort(!!enexpr(sort))
    if (is.null(sort)) {
      res <- list(window = list(op = op), frame = frame)
    } else {
      res <- list(window = list(op = op), frame = frame, sort = list(sort))
    }
    new_virgo_op(res, class = "virgo_window")
  }
}

#' @export
print.virgo_window <- print.virgo_aggregate

#' Interactive window operations
#'
#' @param x A data variable, used in conjunction with `dplyr::mutate()`.
#' @param frame A list/vector of two elements to indicate the number of data values
#' preceding and following the current data object. `NULL` gives unbounded elements
#' proceding or following the current position.
#' @param sort A variable for sorting data within a window in ascending order.
#' `-` before the variable gives descending order. `NULL` disables sorting.
#'
#' @rdname vg-window
#' @export
vg_window_sum <- virgo_window_factory("sum")

#' @rdname vg-window
#' @export
vg_window_mean <- virgo_window_factory("mean")

#' @rdname vg-window
#' @export
vg_window_rank <- virgo_window_factory("rank")

#' @rdname vg-window
#' @export
vg_window_count <- virgo_window_factory("count")

#' @rdname vg-window
#' @export
vg_cumsum <- function(x, sort = NULL) {
  vg_window_sum(x, sort = !!enexpr(sort))
}

#' @rdname vg-window
#' @export
vg_cummean <- function(x, sort = NULL) {
  vg_window_mean(x, sort = !!enexpr(sort))
}

vg_ranking <- function(x, n = 1, sort = NULL, op) {
  sort <- simple_sort(!!enexpr(sort))
  if (is.null(sort)) {
    res <- list(window = list(op = op, param = n))
  } else {
    res <- list(window = list(op = op, param = n), sort = list(sort))
  }
  new_virgo_op(res, class = "virgo_window")
}

#' @rdname vg-window
#' @export
vg_row_number <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "row_number")
}

#' @rdname vg-window
#' @export
vg_rank <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "rank")
}

#' @rdname vg-window
#' @export
vg_dense_rank <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "dense_rank")
}

#' @rdname vg-window
#' @export
vg_percent_rank <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "percent_rank")
}

#' @rdname vg-window
#' @export
vg_cume_dist <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "cume_dist")
}

#' @param n The number of elements.
#'
#' @rdname vg-window
#' @export
vg_ntile <- function(x, n = 1, sort = NULL) {
  vg_ranking(x, n = n, sort = !!enexpr(sort), "ntile")
}

#' @rdname vg-window
#' @export
vg_lead <- function(x, n = 1, sort = NULL) {
  vg_ranking(x, n = n, sort = !!enexpr(sort), "lead")
}

#' @rdname vg-window
#' @export
vg_lag <- function(x, n = 1, sort = NULL) {
  vg_ranking(x, n = n, sort = !!enexpr(sort), "lag")
}

simple_sort <- function(x) {
  x <- enexpr(x)
  if (is.null(x)) {
    NULL
  } else if (is_call(x, "-")) {
    list(field = as_string(call_args(x)[[1]]), order = "descending")
  } else if (!is_call(x, "c")) {
    list(field = as_string(x), order = "ascending")
  } else {
    map(call_args(x), simple_sort)
  }
}

vg_bin <- function(x, base = 10, divide = c(5, 2), extent = NULL, maxbins = 10,
  nice = TRUE, step = NULL) {
  bin <- list(base = base, divide = divide, extent = extent, maxbins = maxbins,
    nice = nice, step = step)
  new_virgo_op(bin, class = "virgo_bin")
}

# mosaic transform, could probably be simplified
vg_mosaic <- function(enc) {
  # the somewhat complex mosaic transform
  list(
    list(
      aggregate = list(list(op = "count", as = "stack_count_x")),
      groupby = list(enc$x$field, enc$y$field)
    ),
    list(
      stack = "stack_count_x",
      groupby = list(),
      as = list("stack_count_x_left", "stack_count_x_right"),
      offset = "normalize",
      sort = list(list(field = enc$x$field, order = "ascending"))
    ),
    list(
      window = list(
        list(op = "min", field = "stack_count_x_left", as = "x"),
        list(op = "max", field = "stack_count_x_right", as = "x2"),
        list(op = "dense_rank", as = "rank_y"),
        list(op = "distinct", field = enc$y$field, as = "distinct_y")
      ),
      groupby = list(enc$x$field),
      frame = c(NA, NA),
      sort = list(list(field = enc$y$field, order = "ascending"))
    ),
    list(
      window = list(
        list(op = "dense_rank", as = "rank_x")
      ),
      frame = c(NA, NA),
      sort = list(list(field = enc$x$field, order = "ascending"))
    ),
    list(
      stack = "stack_count_x",
      groupby = list(enc$x$field),
      as = list("y", "y2"),
      offset = "normalize",
      sort = list(list(field = enc$y$field, order = "ascending"))
    ),
    list(
      calculate = "datum.x + (datum.rank_x - 1) * 0.01",
      as = "nx"
    ),
    list(
      calculate = "datum.x2 + (datum.rank_x - 1) *  0.01",
      as = "nx2"
    ),
    list(
      calculate = "datum.y + (datum.rank_y - 1) * datum.distinct_y * 0.01 / max(datum.distinct_y)",
      as = "ny"
    ),
    list(
      calculate = "datum.y2 + (datum.rank_y - 1) * datum.distinct_y * 0.01 / max(datum.distinct_y)",
      as = "ny2"
    ),
    list(
      calculate = "(datum.nx + datum.nx2) / 2",
      as = "xc"
    ),
    list(
      calculate = "(datum.ny + datum.ny2) / 2",
      as = "yc"
    )
  )
}
