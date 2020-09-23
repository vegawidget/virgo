#' Use a ggplot2 like theme for virgo plots
#' Set a default config that is more similar to what ggplot2 users are familiar
#' with.
#' TODO: figure out how to set minor ticks via config
config <- function() {
  # mark props
  markColor <- "#000"
  circle <- list(filled = TRUE, color = markColor, opacity = 1)
  point <- circle
  shape = list(color = markColor)
  rect <- list(fill = markColor)
  # box-plot uses bar so default will be black instead of usual white
  bar <- rect

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

  scale <- list(
    continuousPadding = 4
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
    circle = circle,
    point = point,
    shape = shape,
    rect = rect,
    bar = rect,
    axis = axis,
    scale = scale,
    range = range,
    legend = legend
  )
}


