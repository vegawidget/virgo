#' Vega theme configurations
#'
#' @param v A `vega()` object.
#' @param background A plot background.
#' @param axix,axis_x,axis_y A named list to define axis.
#' @param header,legend,title,view,facet A named list.
#'
#' @rdname vega-config
#' @export
config <- function(v, background = "white", axis = list(), axis_x = list(),
  axis_y = list(), header = list(), legend = list(), title = list(), 
  view = list(), facet = list()) {
  default <- config_ggplot(v)$config

  fn <- function(x) {
    vec_set_names(x, standardise_names(names(x)))
  }

  axis <- replace(default$axis, names(axis), fn(axis))
  axis_x <- replace(default$axis_x, names(axis_x), fn(axis_x))
  axis_y <- replace(default$axis_y, names(axis_y), fn(axis_y))
  header <- replace(default$header, names(header), fn(header))
  legend <- replace(default$legend, names(legend), fn(legend))
  title <- replace(default$title, names(title), fn(title))
  view <- replace(default$view, names(view), fn(view))
  # concat <- replace(default$concat, names(concat), fn(concat))
  facet <- replace(default$facet, names(facet), fn(facet))

  res <- list(
    background = background,
    axis = axis,
    axisX = axis_x,
    axisY = axis_y,
    header = header,
    legend = legend,
    title = title,
    view = view,
    # concat = concat,
    facet = facet
  )
  old <- default[!vec_in(names(default), names(res))]
  new_virgo(c(unclass(v), list(config = c(old, res))))
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
    axisX = list(),
    axisY = list(),
    header = list(),
    legend = legend,
    title = list()
    # concat = list()
  ))))
}


