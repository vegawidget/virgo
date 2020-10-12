standardise_names <- function(x) {
  if (vec_in("alpha", x)) {
    abort(c("`alpha` is an invalid mark property in vega.",
      i = "Do you mean `opacity`?"))
  }
  x[vec_match("group", x)] <- "details"
  x[vec_match("colour", x)] <- "color"
  gsub('\\_(\\w?)', '\\U\\1', x, perl = TRUE)
}
