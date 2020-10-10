standardise_names <- function(x) {
  x[vec_match("group", x)] <- "details"
  x[vec_match("colour", x)] <- "color"
  gsub('\\_(\\w?)', '\\U\\1', x, perl = TRUE)
}
