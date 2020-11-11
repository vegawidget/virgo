# selection perhaps can be implemented as delayed reactives
# it will never be evaluated in console mode
# a random id needs to be assigned when it's created for composition

# slider <- input_slider(1:10, min, max, step, init)
# bind = c(Year = slider)
# init = c(Cycliners = 4, Year = 1977)
# init = list(x = c(55, 160), y = c(13, 37))

new_virgo_input <- function(x, init = NULL) {
  structure(x, init = init, class = c("virgo_input", "list"))
}
# inputs that are bound to an HTML element are special case of
# single selection
input_slider <- function(min, max, step, init = NULL) {
  new_virgo_input(
    list(input = "range", min = min, max = max, step = step),
    init = init
  )
}
input_radio <- function(choices, labels = NULL, init = NULL) {
  new_virgo_input(
    list(input = "radio", options = choices, labels = labels),
    init = init
  )
}
input_select <- function(choices, labels = NULL, init = NULL) {
  new_virgo_input(
    list(input = "select", options = choices, labels = labels),
    init = init
  )
}

input_factory  <- function(input) {
  force(input)
  function(init = NULL, ...) {
    new_virgo_input(list(input = input, ...), init = init)
  }
}

input_date <- input_factory("date")
input_datetime <- input_factory("datetime")
input_month <- input_factory("month")
input_week <- input_factory("week")
input_color <- input_colour <- input_factory("color")
input_textbox <- input_factory("text")
input_checkbox <- input_factory("checkbox")

select_bind <- function(..., id = NULL) {
  elements <- map(enquos(..., .named = TRUE), eval_tidy)
  stopifnot(all(map_lgl(elements, is_virgo_input)))
  fields <- names(elements)
  if (length(fields) == 1) {
    fields <- list(fields)
  }
  inits <- map(elements, function(.) attr(., "init"))
  # init does not work unless all elements are specified
  if (any(map_lgl(inits, is.null))) {
    inits <- NULL
  }
  binds <- map(elements, unclass)
  if (is.null(id)) {
    id <- rand_id()
  }
  new_virgo_selection(
    list2(
      !!id := list(type = "single",
                   fields = fields,
                   bind = binds,
                   init = inits)
    )
  )
}

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

is_virgo_input <- function(x) {
  inherits(x, "virgo_input")
}


