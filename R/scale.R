#' @importFrom scales log10_trans sqrt_trans date_trans expand_range
#' @title Vega scales
#'
#' @param v A `vega()` object.
#' @param name A string for an axis label. `zap()` is the default label.
#' `NULL` removes the label.
#' @param domain A vector of two elements to define the range.
#' @param type One of "linear", "log", "sqrt", "temporal", "band", "category" scale types.
#' @param breaks One of:
#' * `NULL` for no breaks
#' * `zap()` for default breaks
#' * A vector for custom breaks
#' @param orient One of "bottom" and "top" for `scale_x()`. One of "left" and "right"
#' for `scale_y()`.
#' @param range Custom range specification for colour, opacity, and size.
#' @param ... Other parameters passed to vega specs.
#'
#' @rdname vega-scales
#' @export
scale_x <- function(v, name = zap(), domain = zap(), type = "linear",
  breaks = zap(), orient = "bottom", ...) {
  abort_if_not_virgo(v)
  for (i in seq_along(v$layer)) {
    v$layer[[i]]$encoding$x$scale$type <- type
    data <- v$layer[[i]]$data$values %||% v$data$values
    field <- v$layer[[i]]$encoding$x$field
    v$layer[[i]]$encoding$x$scale$domain <- rescale_domain(data[[field]], type)
    if (!is_zap(breaks)) {
      if (is.null(breaks)) {
        breaks <- list()
      }
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

#' @rdname vega-scales
#' @export
scale_y <- function(v, name = zap(), domain = zap(), type = "linear",
  breaks = zap(), orient = "left", ...) {
  abort_if_not_virgo(v)
  for (i in seq_along(v$layer)) {
    v$layer[[i]]$encoding$y$scale$type <- type
    data <- v$layer[[i]]$data$values %||% v$data$values
    field <- v$layer[[i]]$encoding$y$field
    v$layer[[i]]$encoding$y$scale$domain <- rescale_domain(data[[field]], type)
    if (!is_zap(breaks)) {
      if (is.null(breaks)) {
        breaks <- list()
      }
      v$layer[[i]]$encoding$y$axis$values <- breaks
    }
    if (!is_zap(name)) {
      title <- list(title = name)
      v$layer[[i]]$encoding$y <- c(v$layer[[i]]$encoding$y, title)
    }
    if (!is_zap(domain)) {
      v$layer[[i]]$encoding$y$scale$domain <- interpret_domain(domain)
    }
    v$layer[[i]]$encoding$y$axis$orient <- orient
  }
  v
}

#' @param scheme Colour scheme.
#' @param guide If `FALSE`, remove the legend.
#' @rdname vega-scales
#' @export
scale_color <- function(v, name = zap(), range = zap(), scheme = zap(),
  guide = TRUE, ...) {
  abort_if_not_virgo(v)
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
    if (!guide) {
      legend <- list(legend = NULL)
      v$layer[[i]]$encoding$color <- c(v$layer[[i]]$encoding$color, legend)
    }
    v$layer[[i]]$encoding$color$scale <- c(v$layer[[i]]$encoding$color$scale, dots)
  }
  v
}

#' @rdname vega-scales
#' @export
scale_colour <- scale_color

#' @rdname vega-scales
#' @export
scale_size <- function(v, name = zap(), range = zap(), type = "linear",
  guide = TRUE, ...) {
  abort_if_not_virgo(v)
  dots <- dots_list(..., .named = TRUE, .homonyms = "error")
  dots <- vec_set_names(dots, standardise_names(names(dots)))
  for (i in seq_along(v$layer)) {
    if (!is_zap(name)) {
      title <- list(title = name)
      v$layer[[i]]$encoding$size <- c(v$layer[[i]]$encoding$size, title)
    }
    if (!is_zap(range)) {
      v$layer[[i]]$encoding$size$scale$range <- range
    }
    v$layer[[i]]$encoding$size$scale$type <- type
    if (!guide) {
      legend <- list(legend = NULL)
      v$layer[[i]]$encoding$size <- c(v$layer[[i]]$encoding$size, legend)
    }
    v$layer[[i]]$encoding$size$scale <- c(v$layer[[i]]$encoding$size$scale, dots)
  }
  v
}

scale_opacity <- function(v, name = zap(), range = zap(), type = "linear",
  guide = TRUE, ...) {
  abort_if_not_virgo(v)
  dots <- dots_list(..., .named = TRUE, .homonyms = "error")
  dots <- vec_set_names(dots, standardise_names(names(dots)))
  for (i in seq_along(v$layer)) {
    if (!is_zap(name)) {
      title <- list(title = name)
      v$layer[[i]]$encoding$opacity <- c(v$layer[[i]]$encoding$opacity, title)
    }
    if (!is_zap(range)) {
      v$layer[[i]]$encoding$opacity$scale$range <- range
    }
    v$layer[[i]]$encoding$opacity$scale$type <- type
    if (!guide) {
      legend <- list(legend = NULL)
      v$layer[[i]]$encoding$opacity <- c(v$layer[[i]]$encoding$opacity, legend)
    }
    v$layer[[i]]$encoding$opacity$scale <- c(v$layer[[i]]$encoding$opacity$scale,
      dots)
  }
  v
}

#' @rdname vega-scales
#' @export
scale_shape <- function(v, name = zap(), guide = TRUE, ...) {
  abort_if_not_virgo(v)
  dots <- dots_list(..., .named = TRUE, .homonyms = "error")
  dots <- vec_set_names(dots, standardise_names(names(dots)))
  for (i in seq_along(v$layer)) {
    if (!is_zap(name)) {
      title <- list(title = name)
      v$layer[[i]]$encoding$shape <- c(v$layer[[i]]$encoding$shape, title)
    }
    if (!guide) {
      legend <- list(legend = NULL)
      v$layer[[i]]$encoding$shape <- c(v$layer[[i]]$encoding$shape, legend)
    }
    v$layer[[i]]$encoding$shape$scale <- c(v$layer[[i]]$encoding$shape$scale,
      dots)
  }
  v
}

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

rescale_domain.character <- function(x, type = "ordinal") {
  NULL
}

rescale_domain.factor <- rescale_domain.character

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
