# zap() gives defaults
# NULL removes/disables
scale_x <- function(v, name = zap(), domain = zap(), type = "linear",
  orient = "bottom") {
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
  orient = "left") {
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
