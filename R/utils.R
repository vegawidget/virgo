standardise_encodings <- function(x) {
  if (vec_in("alpha", x)) {
    abort(c("`alpha` is an invalid property.", i = "Do you mean `opacity`?"))
  }
  x[vec_match("group", x)] <- "details"
  x[vec_match("colour", x)] <- "color"
  abort_if_camelCase(x)
  x <- standardise_names(valid_encodings(x))
  x
}

standardise_names <- function(x) {
  abort_if_camelCase(x)
  gsub('\\_(\\w?)', '\\U\\1', x, perl = TRUE)
}

abort_if_camelCase <- function(x) {
  camel_lgl <- grepl("[[:upper:]]", x, perl = TRUE)
  if (any(camel_lgl)) {
    camel_x <- x[camel_lgl][1]
    err <- sprintf("`%s` is an invalid property.", camel_x)
    hint <- gsub("([a-z])([A-Z])", "\\1\\_\\L\\2", camel_x, perl = TRUE)
    abort(c(err, i = sprintf("Do you mean `%s`?", hint)))
  }
}

valid_encodings <- function(x) {
  props <- c("x", "y", "x2", "y2", "details", "fill", "fill_opacity", "color", 
    "size", "opacity", "shape", "angle", "tooltip", "url", "radius", "radius2",
    "stroke", "stroke_opacity", "stroke_cap", "stroke_dash", "stroke_join",
    "stroke_width", "text", "theta", "theta2", "href", "description", "cursor",
    "interpolate")
  lgl <- vec_in(x, props)
  if (!all(lgl)) {
    abort(c("Invalid property:", x[!lgl]))
  }
  x
}

square_brackets <- function(x) {
  if (is.null(x)) {
    x
  } else if (grepl("\\.", x)) {
    paste0("[", x, "]")
  } else {
    x
  }
}

fmt_bullets <- function(x) {
  paste(paste("*", names(x)), x, sep = ": ")
}
