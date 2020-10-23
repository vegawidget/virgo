# zap() gives defaults
# NULL removes/disables
# TODO: args accepted in ... depend on continuous or discrete scales,
# e.g. `nice` in continuous and `format` in temporal
scale_x <- function(v, name = zap(), domain = zap(), type = "linear",
  orient = "bottom", ...) {
  for (i in seq_along(v$layer)) {
    v$layer[[i]]$encoding$x$scale$type <- type
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
  orient = "left", ...) {
  for (i in seq_along(v$layer)) {
    v$layer[[i]]$encoding$y$scale$type <- type
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
