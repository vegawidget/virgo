# selection perhaps can be implemented as delayed reactives
# it will never be evaluated in console mode
# a random id needs to be assigned when it's created for composition

# bind(Year = input_slider(min, max, step, init))
# init = c(Cycliners = 4, Year = 1977)
# init = list(x = c(55, 160), y = c(13, 37))
# `fields` not needed as it goes with `bind`
select_single <- function(encodings = NULL, init = NULL, bind,
  nearest = FALSE, on = "click", clear = "dblclick", empty = "all", resolve) {

}

select_multi <- function(encodings = NULL, init = NULL, bind,
  toggle, nearest = FALSE, on = "click", clear = "dblclick", empty = "all",
  resolve) {

}

select_interval <- function(encodings = c("x", "y"), init = NULL,
  bind, mark, zoom, translate, on = "click", clear = "dblclick", empty = "all",
  resolve) {
  structure(
    list(type = "interval", encodings = encodings),
    name = rand_id(), class = "virgo_selection")
}

rand_id <- function() {
  rand <- as.hexmode(sample(256, 4, replace = TRUE) - 1)
  paste0(rand, collapse = "")
}

color_if <- function(selection, true, false) {
  stopifnot(is_virgo_selection(selection))
  structure(list(selection = selection, encoding = "color",
    true = enquo(true), false = enquo(false)), class = "virgo_condition")
}

is_virgo_selection <- function(x) {
  inherits(x, "virgo_selection")
}

is_virgo_condition <- function(x) {
  inherits(x, "virgo_condition")
}
