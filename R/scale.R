#' @importFrom scales log10_trans sqrt_trans expand_range
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
    v$layer[[i]]$encoding$x$scale$domain <- rescale_domain(data, field, type)
    v$layer[[i]]$encoding$x$axis$values <- rebreak_axis(data, field, type)
    if (!is_zap(breaks)) {
      v$layer[[i]]$encoding$x$axis$values <- breaks
    }
    if (!is_zap(name)) {
      title <- list(title = name) # special case for name = NULL
      v$layer[[i]]$encoding$x <- c(v$layer[[i]]$encoding$x, title)
    }
    if (!is_zap(domain)) {
      if (is_virgo_selection(domain)) {
        v$layer[[i]]$encoding$x$scale$domain <- 
          list(selection = selection_composition(domain))
      } else {
        v$layer[[i]]$encoding$x$scale$domain <- domain
      }
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
    v$layer[[i]]$encoding$y$scale$domain <- rescale_domain(data, field, type)
    v$layer[[i]]$encoding$y$axis$values <- rebreak_axis(data, field, type)
    if (!is_zap(breaks)) {
      v$layer[[i]]$encoding$y$axis$values <- breaks
    }
    if (!is_zap(name)) {
      title <- list(title = name)
      v$layer[[i]]$encoding$y <- c(v$layer[[i]]$encoding$y, title)
    }
    if (!is_zap(domain)) {
      if (is_virgo_selection(domain)) {
        v$layer[[i]]$encoding$y$scale$domain <- 
          list(selection = selection_composition(domain))
      } else {
        v$layer[[i]]$encoding$y$scale$domain <- domain
      }
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

rescale_domain <- function(data, field, type = "linear") {
  x <- data[[field]]
  switch(type,
    "log" = log10_trans()$inverse(expand_domain(log(x, 10))),
    "sqrt" = sqrt_trans()$inverse(expand_domain(sqrt(x))),
    x
  )
}

rebreak_axis <- function(data, field, type = "linear") {
  x <- data[[field]]
  switch(type,
    "log" = log10_trans()$breaks(x),
    "sqrt" = sqrt_trans()$breaks(x),
    x
  )
}

expand_domain <- function(x) {
  rng <- range(x, na.rm = TRUE)
  expand_range(rng, mul = 0.05)
}
