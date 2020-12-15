# selection perhaps can be implemented as delayed reactives
# it will never be evaluated in console mode
# a random id needs to be assigned when it's created for composition

new_virgo_input <- function(x, init = NULL) {
  structure(x, init = init, class = "virgo_input")
}
# inputs that are bound to an HTML element are special case of
# single selection
input_slider <- function(name = NULL, min, max, step, init = NULL) {
  new_virgo_input(
    list(input = "range", min = min, max = max, step = step, name = name),
    init = init
  )
}
input_radio <- function(name = NULL, choices, init = NULL) {
  new_virgo_input(
    list(input = "radio", options = choices, labels = names(choices), name = name),
    init = init
  )
}
input_select <- function(name = NULL, choices, init = NULL) {
  new_virgo_input(
    list(input = "select", options = choices, labels = names(choices), name = name),
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

select_bind <- function(...) {
  elements <- map(enquos(..., .named = TRUE), eval_tidy)
  # FIXME: expect the same type of inputs
  stopifnot(all(map_lgl(elements, is_virgo_input)))
  fields <- list(names(elements))
  inits <- map(elements, function(.) attr(., "init"))
  # init does not work unless all elements are specified
  if (any(map_lgl(inits, is.null))) {
    inits <- NULL
  }
  binds <- map(elements, unclass)
  new_virgo_selection(
    list2(
      !!rand_id() := list(type = "single",
                          fields = fields,
                          bind = binds,
                          init = inits)
    )
  )
}

select_single <- function(encodings = NULL, init = NULL, nearest = FALSE,
  on = "click", clear = "dblclick", empty = "all", resolve = "global") {
  new_virgo_selection(list2(!!rand_id() := list(
    type = "single", encodings = encodings, init = init,
    nearest = nearest, on = on, clear = clear, empty = empty,
    resolve = resolve)))
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

new_virgo_selection <- function(x, composition = NULL, transform = NULL) {
  structure(x, composition = composition, transform = transform,
    class = "virgo_selection")
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

encode_if <- function(selection, true, false) {
  stopifnot(is_virgo_selection(selection))
  new_virgo_condition(list(selection = selection,
    true = enquo(true), false = enquo(false)))
}

new_virgo_condition <- function(x) {
  structure(x, class = "virgo_condition")
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

selection_union <- function(x) {
  # remove duplicated seletions and move all selections to the top layer
  names_sel <- vec_c(!!!map(x, function(x) names(x$selection)))
  if (is.null(names_sel)) { return(x) }
  unique_idx <- vec_match(vec_unique(names_sel), names_sel)
  unnamed_sel <- vec_c(!!!map(x, function(x) x$selection))[unique_idx]
  x[[1]]$selection <- vec_set_names(unnamed_sel, names_sel[unique_idx])
  x <- c(x[1], map(x[-1], function(x) {x$selection <- NULL; x}))
  x
}

mutate.virgo_selection <- function(.data, ...) {
  quos <- enquos(..., .named = TRUE)
  fields <- names(quos)
  window_lst <- eval_tidy(quos[[1]])
  window_lst$window <- list(
    list(op = window_lst$window$op, field = as_field(quos[[1]]), as = fields[1]))
  new_virgo_selection(unclass(.data), .data %@% "composition", window_lst)
}
