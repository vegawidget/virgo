new_virgo_op <- function(x, ..., class) {
  # NOTE: nonstandard class(x) for asJSON()
  structure(x, ..., class = c(class, "virgo_op", class(x)))
}

is_virgo_op <- function(x) {
  inherits(x, "virgo_op")
}

vg_sum <- function(x) {
  stopifnot(!is_missing(x))
  new_virgo_op(x, aggregate = "sum", class = "virgo_aggregate")
}

vg_mean <- function(x) {
  stopifnot(!is_missing(x))
  new_virgo_op(x, aggregate = "mean", class = "virgo_aggregate")
}

vg_count <- function(x) { # "count" can take empty values
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
