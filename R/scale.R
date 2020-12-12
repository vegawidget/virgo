#' @importFrom scales log10_trans sqrt_trans date_trans expand_range breaks_pretty
# zap() gives defaults
# NULL removes/disables
# TODO: args accepted in ... depend on continuous or discrete scales,
# e.g. `nice` in continuous and `format` in temporal
scale_x <- function(v, name = zap(), domain = zap(), type = "linear",
  breaks = zap(), orient = "bottom", ...) {
  for (i in seq_along(v$layer)) {
    v$layer[[i]]$encoding$x$scale$type <- type
    data <- v$layer[[i]]$data$values
    field <- v$layer[[i]]$encoding$x$field
    v$layer[[i]]$encoding$x$scale$domain <- rescale_domain(data[[field]], type)
    v$layer[[i]]$encoding$x$axis$values <- rebreak_axis(data[[field]], type)
    if (!is_zap(breaks)) {
      v$layer[[i]]$encoding$x$axis$values <- breaks
    }
    if (!is_zap(name)) {
      title <- list(title = name) # special case for name = NULL
      v$layer[[i]]$encoding$x <- c(v$layer[[i]]$encoding$x, title)
    }
    if (!is_zap(domain)) {
      # rescale_domain(domain, type) includes points outside of specified domain
      v$layer[[i]]$encoding$x$scale$domain <- interpret_domain(domain)
    }
    v$layer[[i]]$encoding$x$axis$orient <- orient
  }
  v
}

scale_y <- function(v, name = zap(), domain = zap(), type = "linear",
  breaks = zap(), orient = "left", ...) {
  for (i in seq_along(v$layer)) {
    v$layer[[i]]$encoding$y$scale$type <- type
    data <- v$layer[[i]]$data$values
    field <- v$layer[[i]]$encoding$y$field
    v$layer[[i]]$encoding$y$scale$domain <- rescale_domain(data[[field]], type)
    v$layer[[i]]$encoding$y$axis$values <- rebreak_axis(data[[field]], type)
    if (!is_zap(breaks)) {
      v$layer[[i]]$encoding$y$axis$values <- breaks
    }
    if (!is_zap(name)) {
      title <- list(title = name)
      v$layer[[i]]$encoding$y <- c(v$layer[[i]]$encoding$y, title)
    }
    if (!is_zap(domain)) {
      v$layer[[i]]$encoding$y$scale$domain <- rescale_domain(domain, type)
    }
    v$layer[[i]]$encoding$y$axis$orient <- orient
  }
  v
}

scale_color <- function(v, name = zap(), range = zap(), scheme = zap(), ...) {
  dots <- dots_list(..., .named = TRUE, .homonyms = "error")
  dots <- vec_set_names(dots, standardise_names(names(dots)))
  for (i in seq_along(v$layer)) {
    if (!is_zap(name)) {
      title <- list(title = name)
      v$layer[[i]]$encoding$color <- c(v$layer[[i]]$encoding$color, title)
    }
    if (!is_zap(range)) {
      v$layer[[i]]$encoding$color$scale$range <- range
    }
    if (!is_zap(scheme)) {
      v$layer[[i]]$encoding$color$scale$range <- NULL
      v$layer[[i]]$encoding$color$scale$scheme <- scheme
    }
    v$layer[[i]]$encoding$color$scale <- c(v$layer[[i]]$encoding$color$scale, dots)
  }
  v
}

scale_colour <- scale_color

rescale_domain <- function(x, type = "linear") {
  UseMethod("rescale_domain")
}

rescale_domain.default <- function(x, type = "linear") {
  switch(type,
    "log" = log10_trans()$inverse(expand_domain(log10_trans()$transform(x))),
    "sqrt" = sqrt_trans()$inverse(expand_domain(sqrt_trans()$transform(x))),
    expand_domain(x)
  )
}

rescale_domain.Date <- function(x, type = "time") {
  domain <- date_trans()$inverse(expand_domain(date_trans()$transform(x)))
  interpret_domain(domain)
}

rebreak_axis <- function(x, type = "linear") {
  UseMethod("rebreak_axis")
}

rebreak_axis.default <- function(x, type = "linear") {
  NULL
}

rebreak_axis.numeric <- function(x, type = "linear") {
  switch(type,
    "log" = log10_trans()$breaks(x),
    "sqrt" = sqrt_trans()$breaks(x),
    breaks_pretty()(x)
  )
}

rebreak_axis.Date <- function(x, type = "linear") {
  unname(interpret_domain(breaks_pretty()(x)))
}

expand_domain <- function(x) {
  rng <- range(x, na.rm = TRUE)
  expand_range(rng, mul = 0.05)
}

interpret_domain <- function(x) {
  UseMethod("interpret_domain")
}

interpret_domain.default <- function(x) {
  x
}

interpret_domain.virgo_selection <- function(x) {
  list(selection = selection_composition(x)) 
}

interpret_domain.Date <- function(x) {
  lst <- as.POSIXlt(x)
  map(lst, function(x) 
    list(year = 1900 + x$year, month = x$mon + 1, date = x$mday))
}

interpret_domain.POSIXt <- function(x) {
  lst <- as.POSIXlt(x)
  map(lst, function(x) 
    list(year = 1900 + x$year, month = x$mon + 1, date = x$mday,
      hours = x$hour, minutes = x$min, seconds = x$sec %/% 1,
      milliseconds = (x$sec %% 1 * 1000) %/% 1))
}
