#' Vega theme configurations
#'
#' @param v A `vega()` object.
#' @param background A plot background.
#' @param axix,axis_x,axis_y A named list to define axis.
#' @param header,legend,title,view,concat,facet A named list.
#'
#' @rdname vega-config
#' @export
config <- function(v, background = "white", axis = list(), axis_x = list(),
  axis_y = list(), header = list(), legend = list(), title = list(), 
  view = list(), concat = list(), facet = list()) {
  abort_if_not_virgo(v)

  fn <- function(x) {
    vec_set_names(x, standardise_names(names(x)))
  }

  res <- list(
    background = background,
    axis = fn(axis),
    axis_x = fn(axis_x),
    axis_y = fn(axis_y),
    headerRow = list(labelOrient = "right", titleOrient = "right"),
    header = fn(header),
    legend = fn(legend),
    title = fn(title),
    view = fn(view),
    concat = fn(concat),
    facet = fn(facet)
  )
  new_virgo(c(unclass(v), list(config = res)))
}

#' @rdname vega-config
#' @export
config_ggplot <- function(v) {
  abort_if_not_virgo(v)
  # mark props
  mark_color <- "#000"
  point <- circle <- square <- list(color = mark_color, opacity = 1, size = 60)
  line <- tick <- trail <- geoshape <- list(color = mark_color)
  bar <- area <- rect <- list(fill = "#595959")
  # box-plot uses bar so default will be black instead of usual white

  # axis props
  axis <- list(
    domain = FALSE,
    domainColor = "#FFFFFFF",
    grid = TRUE,
    gridColor = "#FFFFFF",
    gridOpacity = 1,
    labelColor = "#7F7F7F",
    labelPadding = 4,
    tickColor = "#7F7F7F",
    tickSize = 5.67
  )

  legend <- list(
    orient = "right",
    padding = 1
  )

  new_virgo(c(unclass(v), list(config = list(
    view = list(fill = "#e5e5e5"), # sets inner view to grey,
    facet = list(spacing = 5),
    headerRow = list(labelOrient = "right", titleOrient = "right"),
    circle = circle,
    point = point,
    line = line,
    trail = trail,
    tick = tick,
    geoshape = geoshape,
    square = square,
    rect = rect,
    area = area,
    bar = bar,
    axis = axis,
    legend = legend
  ))))
}


