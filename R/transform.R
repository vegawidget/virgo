new_virgo_op <- function(x, ..., class) {
  structure(x, ..., class = c(class, "virgo_op"))
}

is_virgo_op <- function(x) {
  inherits(x, "virgo_op")
}

virgo_op <- function() {
  c("vg_sum", "vg_mean", "vg_count", "vg_distinct", "vg_median", "vg_min",
    "vg_max",
    "vg_year", "vg_quarter", "vg_month", "vg_date", "vg_week", "vg_day",
    "vg_dayofyear", "vg_hours", "vg_minutes", "vg_seconds", "vg_milliseconds")
}

virgo_aggregate_factory <- function(aggregate) {
  function(x) {
    stopifnot(!is_missing(x))
    new_virgo_op(x, aggregate = aggregate, class = "virgo_aggregate")
  }
}

vg_sum <- virgo_aggregate_factory("sum")
vg_min <- virgo_aggregate_factory("min")
vg_max <- virgo_aggregate_factory("max")
vg_mean <- virgo_aggregate_factory("mean")
vg_median <- virgo_aggregate_factory("median")
vg_distinct <- virgo_aggregate_factory("distinct")

vg_count <- function(x) {
  if (is_missing(x)) { # "count" can take empty values
    x <- list()
  }
  new_virgo_op(x, aggregate = "count", class = "virgo_aggregate")
}

virgo_timeunit_factory <- function(unit) {
  force(unit)
  function(x, step = 1, utc = FALSE) {
    stopifnot(!is_missing(x))
    new_virgo_op(x, timeUnit = unit, step = step, utc = utc, 
      class = "virgo_timeunit")
  }
}

vg_year <- virgo_timeunit_factory("year")
vg_quarter <- virgo_timeunit_factory("quarter")
vg_month <- virgo_timeunit_factory("month")
vg_date <- virgo_timeunit_factory("date")
vg_week <- virgo_timeunit_factory("week")
vg_day <- virgo_timeunit_factory("day")
vg_dayofyear <- virgo_timeunit_factory("dayofyear")
vg_hours <- virgo_timeunit_factory("hours")
vg_minutes <- virgo_timeunit_factory("minutes")
vg_seconds <- virgo_timeunit_factory("seconds")
vg_milliseconds <- virgo_timeunit_factory("milliseconds")
