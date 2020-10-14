# selection perhaps can be implemented as delayed reactives
# it will never be evaluated in console mode
# a random id needs to be assigned when it's created for composition

# slider <- input_slider(1:10, min, max, step, init)
# bind = c(Year = slider)
# init = c(Cycliners = 4, Year = 1977)
# init = list(x = c(55, 160), y = c(13, 37))
select_single <- function(encodings = NULL, init = NULL, bind = NULL,
  nearest = FALSE, on = "click", clear = "dblclick", empty = "all",
  resolve = "global") {
  new_virgo_selection(list2(!!rand_id() := list(
    type = "single", encodings = encodings, init = init, 
    fields = names(fields), bind = bind, nearest = nearest,
    on = on, clear = clear, empty = empty, resolve = resolve)))
}

select_multi <- function(encodings = NULL, init = NULL,
  toggle = TRUE, nearest = FALSE, on = "click", clear = "dblclick",
  empty = "all", resolve = "global") {
  new_virgo_selection(list2(!!rand_id() := list(
    type = "multi", encodings = encodings, init = init, toggle = toggle,
    nearest = nearest, on = on, clear = clear, empty = empty,
    resolve = resolve)))
}

select_interval <- function(encodings = c("x", "y"), init = NULL,
  mark = NULL, on = "[mousedown, window:mouseup] > window:mousemove!",
  clear = "dblclick", translate = on, empty = "all", zoom = TRUE,
  resolve = "global") {
  if (!is.null(mark)) {
    mark <- vec_set_names(mark, standardise_names(names(mark)))
  }
  mark <- as.list(mark)
  new_virgo_selection(list2(!!rand_id() := list(
    type = "interval", encodings = encodings, init = init, mark = mark,
    on = on, clear = clear, translate = translate, empty = empty, zoom = zoom,
    resolve = resolve)))
}

select_legend <- function(field, on = "click", clear = "dblclick") {
  # vega only supports legend bindings for one field or channel
  field <- as.list(simple_select(!!enexpr(field)))
  stopifnot(has_length(field, 1))
  new_virgo_selection(list2(!!rand_id() := list(
    type = "multi", fields = field, bind = "legend")))
}

select_domain <- function() {
  new_virgo_selection(list2(!!rand_id() := list(
    type = "interval", bind = "scales")))
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
  rand <- c("id", as.character(as.hexmode(sample(256, 4, replace = TRUE) - 1)))
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

fill_if <- virgo_condition_factory("fill")
size_if <- virgo_condition_factory("size")
shape_if <- virgo_condition_factory("shape")
color_if <- virgo_condition_factory("color")
colour_if <- virgo_condition_factory("color")
stroke_if <- virgo_condition_factory("stroke")
opacity_if <- virgo_condition_factory("opacity")
fill_opacity_if <- virgo_condition_factory("fillOpacity")
stroke_opacity_if <- virgo_condition_factory("strokeOpacity")

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
