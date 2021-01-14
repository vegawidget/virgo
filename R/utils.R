standardise_names <- function(x) {
  if (vec_in("alpha", x)) {
    abort(c("`alpha` is an invalid channel.", i = "Do you mean `opacity`?"))
  }
  camel_lgl <- grepl("[[:upper:]]", x, perl = TRUE)
  if (any(camel_lgl)) {
    camel_x <- x[camel_lgl][1]
    err <- sprintf("`%s` is an invalid channel.", camel_x)
    hint <- gsub("([a-z])([A-Z])", "\\1\\_\\L\\2", camel_x, perl = TRUE)
    abort(c(err, i = sprintf("Do you mean `%s`?", hint)))
  }
  x[vec_match("group", x)] <- "details"
  x[vec_match("colour", x)] <- "color"
  chn <- c("x", "y", "x2", "y2", "details", "fill", "fill_opacity", "color", 
    "size", "opacity", "shape")
  lgl <- vec_in(x, chn)
  if (!all(lgl)) {
    abort(c("Invalid channel:", x[!lgl]))
  }
  gsub('\\_(\\w?)', '\\U\\1', x, perl = TRUE)
}

square_brackets <- function(x) {
  if (grepl("\\.", x)) {
    paste0("[", x, "]")
  } else {
    x
  }
}

fmt_bullets <- function(x) {
  paste(paste("*", names(x)), x, sep = ": ")
}
