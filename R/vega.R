#' @import rlang tidyselect vctrs
#' @importFrom vegawidget as_vegaspec vega_embed vega_schema vegawidget
#' @importFrom jsonlite write_json
#' @importFrom stats complete.cases lag

new_virgo <- function(spec) {
  structure(spec, class = "virgo")
}

#' Create a new vega visualisation
#'
#' @param data A data frame.
#' @param encoding A list of aethetic encodings via [`enc()`].
#' @param width,height Data plotting width and height.
#'
#' @export
vega <- function(data = NULL, encoding = enc(), width = 300, height = 300) {
  spec <- list(
    data = list(values = data), encoding = encoding,
    width = width, height = height)
  new_virgo(spec)
}

#' @export
as.list.virgo <- function(x, ...) {
  unclass(as_vegaspec(x, ...))
}

#' @export
as.list.virgo_concat <- function(x, ...) {
  unclass(as_vegaspec(x, ...))
}

#' @export
as_vegaspec.virgo_concat <- function(spec, ...) {
  spec_header <- list(`$schema` = vega_schema())
  # vega concat can only use one config
  config <- spec[[1]][[1]]$config
  # clean duplicates
  spec[[1]] <- map(spec[[1]], function(x) {x$config <- NULL; x})
  spec[[1]] <- map(spec[[1]], function(x) {x$`$schema` <- NULL; x})
  # vega concat can only use one config
  spec$config <- config
  as_vegaspec(c(spec_header, spec))
}

#' @export
as_vegaspec.virgo <- function(spec, ...) {
  if (!has_name(spec, "layer")) {
    spec <- mark_blank(spec)
  }
  spec_header <- list(`$schema` = vega_schema())
  spec$data$dir <- NULL
  if (!has_name(spec, "config")) {
    spec <- config_ggplot(spec)
  }
  spec <- unclass(spec)
  if (is.null(spec$data$values) && is.null(spec$data$url)) {
    spec$data <- NULL
  }
  # remove top-level encoding & transform, since it already applies to each layer
  spec$encoding <- spec$transform <- NULL
  # unify default scale domains
  layer <- spec$layer
  xs <- map(layer, function(x)
    c(x$encoding$x$scale$domain, x$encoding$x2$scale$domain))
  ys <- map(layer, function(x)
    c(x$encoding$y$scale$domain, x$encoding$y2$scale$domain))
  xrng <- vec_c(!!!xs)
  yrng <- vec_c(!!!ys)
  if (!is.null(xrng) && !has_name(xrng, "selection") && !is_bare_list(xrng)) {
    xrng <- range(xrng)
  }
  if (!is.null(yrng) && !has_name(yrng, "selection") && !is_bare_list(yrng)) {
    yrng <- range(yrng)
  }
  for (i in seq_along(layer)) {
    if (!is.null(layer[[i]]$encoding$x$scale$domain)) {
      spec$layer[[i]]$encoding$x$scale$domain <- xrng
    }
    if (!is.null(layer[[i]]$encoding$y$scale$domain)) {
      spec$layer[[i]]$encoding$y$scale$domain <- yrng
    }
  }
  spec$layer <- selection_union(spec$layer)
  # facet is used
  if (has_name(spec, "facet")) {
    data <- spec$data$values
    rowvars <- spec$facet$row$field
    colvars <- spec$facet$col$field
    nrows <- vec_unique_count(data[rowvars])
    ncols <- vec_unique_count(data[colvars])
    spec$layer <- map(layer, function(x) { x$data <- NULL; x })
    spec$spec$layer <- spec$layer
    spec$spec$width <- spec$width / ncols
    spec$spec$height <- spec$height / nrows
    spec$layer <- NULL
  }

  if (has_name(spec, "repeat")) {
    # repeater causes there to be a spec object
    spec_header$`repeat` <- spec$`repeat`
    spec_header$config <- spec$config
    pos <- which(names(spec) %in% c("repeat", "config"))
    spec <- list(spec = spec[-pos])
  }

  as_vegaspec(c(spec_header, spec))
}

#' @export
print.virgo <- function(x, renderer = "canvas", ...) {
  renderer <- arg_match(renderer, c("canvas", "svg"))
  print(vegawidget(as_vegaspec(x),
    embed = vega_embed(renderer = renderer, actions = FALSE),
    base_url = x$data$dir), ...)
  invisible(x)
}

#' @export
format.virgo <- function(x, ...) {
  x <- as_vegaspec(x)
  format(x, ...)
}

#' @param spec vega spec.
#' @param ... Options passed to knitr.
#' @param renderer One of "svg" or "canvas".
#' @param options Options.
#' @rdname knit_print.vegaspec
#' @export
knit_print.virgo <- function(spec, ..., renderer = "canvas", options = NULL) {
  spec <- vegawidget(as_vegaspec(spec),
    embed = vega_embed(renderer = renderer, actions = FALSE),
    base_url = spec$data$dir)
  knitr::knit_print(spec, ..., options = options)
}

#' Modify vega title, subtitle, and description
#'
#' @param v A `vega()` object.
#' @param title,subtitle,description Strings.
#'
#' @export
entitle <- function(v, title = NULL, subtitle = NULL, description = NULL) {
  # NOTE: leave all styling properties to `config()`
  abort_if_not_virgo(v)
  v$title <- list(text = title, subtitle = subtitle)
  v$description <- description
  v
}

is_virgo <- function(v) {
  inherits(v, "virgo")
}

abort_if_not_virgo <- function(v) {
  if (!is_virgo(v)) {
    abort("Must be a `vega()` object.")
  }
}

#' Serialise data
#'
#' @inheritParams entitle
#' @param path Directory to save inlining data to external data files.
#'
#' @rdname vega-seralise
#' @export
vega_serialise_data <- function(v, path = NULL) {
  # TODO: args for iso datetime?
  abort_if_not_virgo(v)
  seq_layer <- seq_along(v$layer)
  if (is.null(path)) {
    path <- tempdir()
    top_file <- tempfile("data0", path, ".json")
    layer_file <- tempfile(paste0("data", seq_layer), path, ".json")
  } else {
    path <- normalizePath(path)
    top_file <- file.path(path, "data0.json")
    layer_file <- file.path(path, paste0("data", seq_layer, ".json"))
  }
  v <- write_out_to(v, top_file) # top-level
  for (i in seq_layer) {
    v$layer[[i]] <- write_out_to(v$layer[[i]], layer_file[i])
  }
  v$data$dir <- path
  v
}

#' @rdname vega-seralise
#' @export
vega_serialize_data <- vega_serialise_data

write_out_to <- function(layer, path) {
  if (is.null(layer$data$values)) return(layer)

  if (file.exists(path)) {
    abort("File exists!")
  }
  write_json(layer$data$values, path)
  layer$data$values <- NULL
  layer$data$url <- basename(path)
  layer
}
