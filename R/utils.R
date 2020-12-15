standardise_names <- function(x) {
  if (vec_in("alpha", x)) {
    abort(c("`alpha` is an invalid mark property.",
      i = "Do you mean `opacity`?"))
  }
  camel_lgl <- grepl("[[:upper:]]", x, perl = TRUE)
  if (any(camel_lgl)) {
    camel_x <- x[camel_lgl][1]
    err <- sprintf("`%s` is an invalid mark property.", camel_x)
    hint <- gsub("([a-z])([A-Z])", "\\1\\_\\L\\2", camel_x, perl = TRUE)
    abort(c(err, i = sprintf("Do you mean `%s`?", hint)))
  }
  x[vec_match("group", x)] <- "details"
  x[vec_match("colour", x)] <- "color"
  gsub('\\_(\\w?)', '\\U\\1', x, perl = TRUE)
}

square_brackets <- function(x) {
  if (grepl("\\.", x)) {
    paste0("[", x, "]")
  } else {
    x
  }
}
