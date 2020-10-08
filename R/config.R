# Use a ggplot2 like theme for virgo plots
# Set a default config that is more similar to what ggplot2 users are familiar
# with.
# TODO: figure out how to set minor ticks via config
config_default <- function() {

}

config_ggplot <- function() {
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

  # default palette
  range <- list(
    # ggplot2 uses default hue_pal in scales package,
    # instead just use okabe-ito palette in base graphics
    category = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                 "#0072B2", "#D55E00", "#CC79A7", "#000000")
  )

  legend <- list(
    orient = "right",
    padding = 1
  )

  list(
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
    range = range,
    legend = legend
  )
}


