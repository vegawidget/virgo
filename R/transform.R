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

vg_sum <- virgo_aggregate_factory("sum")
vg_min <- virgo_aggregate_factory("min")
vg_max <- virgo_aggregate_factory("max")
vg_mean <- virgo_aggregate_factory("mean")
vg_median <- virgo_aggregate_factory("median")
vg_count <- virgo_aggregate_factory("count")
vg_distinct <- virgo_aggregate_factory("distinct")

vg_argmin <- function(x, y) {
  new_virgo_op(list(y = y, aggregate = "argmin"), class = "virgo_aggregate")
}

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

vg_year <- virgo_timeunit_factory("year")
vg_quarter <- virgo_timeunit_factory("quarter")
vg_month <- virgo_timeunit_factory("month")
vg_yearmonth <- virgo_timeunit_factory("yearmonth")
vg_date <- virgo_timeunit_factory("date")
vg_week <- virgo_timeunit_factory("week")
vg_day <- virgo_timeunit_factory("day")
vg_dayofyear <- virgo_timeunit_factory("dayofyear")
vg_hours <- virgo_timeunit_factory("hours")
vg_minutes <- virgo_timeunit_factory("minutes")
vg_seconds <- virgo_timeunit_factory("seconds")
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

vg_window_sum <- virgo_window_factory("sum")
vg_window_mean <- virgo_window_factory("mean")
vg_window_rank <- virgo_window_factory("rank")
vg_window_count <- virgo_window_factory("count")

vg_cumsum <- function(x, sort = NULL) {
  vg_window_sum(x, sort = !!enexpr(sort))
}

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

vg_row_number <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "row_number")
}

vg_rank <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "rank")
}

vg_dense_rank <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "dense_rank")
}

vg_percent_rank <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "percent_rank")
}

vg_cume_dist <- function(x, sort = NULL) {
  vg_ranking(x, n = 0, sort = !!enexpr(sort), "cume_dist")
}

vg_ntile <- function(x, n = 1, sort = NULL) {
  vg_ranking(x, n = n, sort = !!enexpr(sort), "ntile")
}

vg_lead <- function(x, n = 1, sort = NULL) {
  vg_ranking(x, n = n, sort = !!enexpr(sort), "lead")
}

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
