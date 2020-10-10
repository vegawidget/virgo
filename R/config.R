config_vega <- function(v) {
  new_virgo(c(unclass(v), list(config = list())))
}

config_ggplot <- function(v) {
  # mark props
  mark_color <- "#000"
  point <- circle <- square <- list(color = mark_color, opacity = 1, size = 60)
  line <- tick <- trail <- shape <- list(color = mark_color)
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
    shape = shape,
    square = square,
    rect = rect,
    area = area,
    bar = bar,
    axis = axis,
    legend = legend
  ))))
}


