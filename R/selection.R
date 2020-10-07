# selection perhaps can be implemented as delayed reactives
# it will never be evaluated in console mode
# a random id needs to be assigned when it's created for composition

# bind(Year = input_slider(min, max, step, init))
# init = c(Cycliners = 4, Year = 1977)
# init = list(x = c(55, 160), y = c(13, 37))
select_single <- function(encodings = NULL, init = NULL, fields = NULL,
  bind = NULL, nearest = FALSE, on = "click", clear = "dblclick", empty = "all",
  resolve = "global") {
  init <- as.list(init)
  fields <- as.list(fields)
  new_virgo_selection(
    list2(!!rand_id() := list(type = "single", encodings = encodings,
      init = init, fields = fields, bind = bind, nearest = nearest,
      on = on, clear = clear, empty = empty, resolve = resolve)))
}

select_multi <- function(encodings = NULL, init = NULL, fields = NULL,
  bind = NULL, toggle = TRUE, nearest = FALSE, on = "click", clear = "dblclick",
  empty = "all", resolve = "global") {
  init <- as.list(init)
  fields <- as.list(fields)
  new_virgo_selection(
    list2(!!rand_id() := list(type = "multi", encodings = encodings,
      init = init, fields = fields, bind = bind, toggle = toggle,
      nearest = nearest, on = on, clear = clear, empty = empty,
      resolve = resolve)))
}

select_interval <- function(encodings = c("x", "y"), init = NULL, fields = NULL,
  bind = NULL, mark = NULL,
  on = "[mousedown, window:mouseup] > window:mousemove!",
  clear = "dblclick", translate = on, empty = "all", zoom = TRUE,
  resolve = "global") {
  init <- as.list(init)
  fields <- as.list(fields)
  mark <- as.list(mark)
  new_virgo_selection(
    list2(!!rand_id() := list(type = "interval", encodings = encodings,
      init = init, fields = fields, bind = bind, mark = mark, on = on,
      clear = clear, translate = translate, empty = empty, zoom = zoom,
      resolve = resolve)))
}

select_legend <- function(fields) {
  select_multi(fields = fields, bind = "legend")
}

new_virgo_selection <- function(x, composition = NULL) {
  structure(x, composition = composition, class = "virgo_selection")
}

#' @export
Ops.virgo_selection <- function(e1, e2) {
  e1_comp <- selection_composition(e1)
  if (.Generic == "&") {
    new_virgo_selection(c(e1, e2),
      composition = list(and = c(e1_comp, selection_composition(e2))))
  } else if (.Generic == "|") {
    new_virgo_selection(c(e1, e2),
      composition = list(or = c(e1_comp, selection_composition(e2))))
  } else if (.Generic == "!") {
    new_virgo_selection(e1, composition = list(not = e1_comp))
  } else {
    abort("Oops")
  }
}

selection_composition <- function(x) {
  (x %@% "composition") %||% names(x)
}

rand_id <- function() {
  rand <- as.hexmode(sample(256, 4, replace = TRUE) - 1)
  paste0(rand, collapse = "")
}

virgo_condition_factory <- function(encoding = "color") {
  force(encoding)
  function(selection, true, false) {
    stopifnot(is_virgo_selection(selection))
    new_virgo_condition(list(list(selection = selection,
      true = enquo(true), false =  enquo(false), encoding = encoding)))
  }
}

new_virgo_condition <- function(x) {
  structure(x, class = "virgo_condition")
}

color_if <- virgo_condition_factory("color")
size_if <- virgo_condition_factory("size")

#' @export
c.virgo_condition <- function(...) {
  lst <- vec_c(!!!map(list2(...), unclass))
  sel_name <- unique(map(lst, function(x) selection_composition(x$selection)))
  stopifnot(has_length(sel_name, 1))
  new_virgo_condition(lst)
}

is_virgo_selection <- function(x) {
  inherits(x, "virgo_selection")
}

is_virgo_condition <- function(x) {
  inherits(x, "virgo_condition")
}
