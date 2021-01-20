# selection perhaps can be implemented as delayed reactives
# it will never be evaluated in console mode
# a random id needs to be assigned when it's created for composition

new_virgo_input <- function(x, init = NULL) {
  structure(x, init = init, class = "virgo_input")
}

# inputs that are bound to an HTML element are special case of
# single selection
input_factory <- function(input) {
  force(input)
  function(init = NULL, ...) {
    new_virgo_input(list(input = input, ...), init = init)
  }
}

#' HTML elements that bind to selections
#'
#' @param name Name of the HTML input.
#' @param min,max Minimum and maximum values. 
#' @param step Incremental step.
#' @param init An initial value.
#' @param choices A (named) vector of options.
#' @param ... Not sure.
#'
#' @rdname vega-input
#' @export
input_slider <- function(name = NULL, min, max, step, init = NULL) {
  new_virgo_input(
    list(input = "range", min = min, max = max, step = step, name = name),
    init = init
  )
}

#' @rdname vega-input
#' @export
input_radio <- function(name = NULL, choices, init = NULL) {
  new_virgo_input(
    list(input = "radio", options = choices, labels = names(choices),
      name = name), init = init)
}

#' @rdname vega-input
#' @export
input_select <- function(name = NULL, choices, init = NULL) {
  new_virgo_input(
    list(input = "select", options = choices, labels = names(choices),
      name = name), init = init)
}

#' @rdname vega-input
#' @export
input_textbox <- input_factory("text")

#' @rdname vega-input
#' @export
input_checkbox <- input_factory("checkbox")

#' @rdname vega-input
#' @export
input_color <- input_factory("color")

#' @rdname vega-input
#' @export
input_colour <- input_factory("color")

#' @rdname vega-input
#' @export
input_date <- input_factory("date")

#' @rdname vega-input
#' @export
input_datetime <- input_factory("datetime")

#' @rdname vega-input
#' @export
input_month <- input_factory("month")

#' @rdname vega-input
#' @export
input_week <- input_factory("week")

#' @export
print.virgo_selection <- function(x, ...) {
  lst <- map(x, function(x) paste(paste0("  ", fmt_bullets(x)), sep = "\n"))
  map2(names(lst), lst, function(x, y) cat(x, y, sep = "\n"))
  invisible(x)
}

#' Initiate a selection
#'
#' @section Composing Multiple Selections:
#' A set of operations ...
#'
#' @param encodings A character vector of encoding channels, such as "x" and "y".
#' @param fields A character vector of data fields.
#' @param init An initial value upon selection.
#' @param nearest If `FALSE`, data values must be interacted with directly to
#' be added to the selection.
#' @param on,clear An event type that triggers/clears the selection. Options are
#' "click", "dblclick", "dragenter", "dragleave", "dragover", "keydown", "keypress",
#' "keyup", "mousedown", "mouseover", "mousemove", "mouseout", "mouseup",
#' "mousewheel", "touchend", "touchmove", "touchstart", "wheel".
#' @param empty An empty selection includes "all" or "none" data values.
#' @param resolve One of "global", "union", "intersect" options to resolve
#' ambiguity for layered and multi-view displays.
#' @param toggle A logical to control whether data values should be toggled or
#' only ever inserted into multi selections.
#' @param mark A named vector of mark properties for brushed rectangle.
#' @param translate A string or logical to interactively move an interval
#' selection back-and-forth.
#' @param zoom If `TRUE`, interactively resize an interval selection.
#' @param ... A set of name-value pairs with data variables on the LHS and
#' `input_*()` on the RHS.
#'
#' @rdname vega-selection
#' @export
select_single <- function(encodings = NULL, fields = NULL, init = NULL,
  nearest = FALSE, on = "click", clear = "dblclick", empty = "all",
  resolve = "global") {
  if (!is.null(fields)) {
    fields <- as.list(fields)
  }
  new_virgo_selection(list2(!!rand_id() := list(
    type = "single", encodings = encodings, fields = fields, init = init,
    nearest = nearest, on = on, clear = clear, empty = empty,
    resolve = resolve)))
}

#' @rdname vega-selection
#' @export
select_multi <- function(encodings = NULL, fields = NULL, init = NULL,
  toggle = TRUE, nearest = FALSE, on = "click", clear = "dblclick",
  empty = "all", resolve = "global") {
  if (!is.null(fields)) {
    fields <- as.list(fields)
  }
  new_virgo_selection(list2(!!rand_id() := list(
    type = "multi", encodings = encodings, fields = fields, init = init,
    toggle = toggle, nearest = nearest, on = on, clear = clear, empty = empty,
    resolve = resolve)))
}

#' @rdname vega-selection
#' @export
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

#' @rdname vega-selection
#' @export
select_legend <- function(fields, on = "click", clear = "dblclick") {
  # vega only supports legend bindings for one field or channel
  field <- as.list(simple_select(!!enexpr(field)))
  stopifnot(has_length(fields, 1))
  new_virgo_selection(list2(!!rand_id() := list(
    type = "multi", fields = fields, bind = "legend")))
}

#' @rdname vega-selection
#' @export
select_domain <- function() {
  new_virgo_selection(list2(!!rand_id() := list(
    type = "interval", bind = "scales")))
}

#' @rdname vega-selection
#' @export
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
    list2(!!rand_id() := list(type = "single", fields = fields, bind = binds,
      init = inits)))
}

new_virgo_selection <- function(x, composition = NULL, transform = NULL,
  groupby = NULL) {
  structure(x, composition = composition, transform = transform,
    groupby = groupby, class = "virgo_selection")
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

#' Conditional encoding selection
#'
#' @param selection A selection or selection compositions.
#' @param true,false Values for true/false element of `selection`.
#'
#' @export
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
  x <- c(x[1], map(x[-1], function(x) { x$selection <- NULL; x }))
  x
}

group_by.virgo_selection <- function(.data, ...) {
  vars <- list(map_chr(enexprs(...), as_string))
  new_virgo_selection(unclass(.data), .data %@% "composition",
    .data %@% "transform", groupby = vars)
}

mutate.virgo_selection <- function(.data, ...) {
  quos <- enquos(..., .named = TRUE)
  fields <- names(quos)
  lst <- eval_trans_mask(quos)
  by <- .data %@% "groupby"
  res <- vec_init_along(lst)
  for (i in seq_along(res)) {
    res[[i]] <- translate(lst[[i]], quos[[i]], fields[[i]], by)
  }
  new_virgo_selection(unclass(.data), .data %@% "composition", res)
}

summarise.virgo_selection <- function(.data, ...) {
  quos <- enquos(..., .named = TRUE)
  fields <- names(quos)
  lst <- eval_trans_mask(quos)
  by <- .data %@% "groupby"
  res <- vec_init_along(lst)
  for (i in seq_along(res)) {
    res[[i]] <- translate_aggregate(lst[[i]], quos[[i]], fields[[i]], by)
  }
  new_virgo_selection(unclass(.data), .data %@% "composition", res)
}

summarize.virgo_selection <- summarise.virgo_selection

translate_aggregate <- function(x, quo, field, by) {
  x$aggregate <- list(
    list(op = x$aggregate, field = as_field(quo), as = field))
  x$groupby <- by
  list(x = unclass(x), as = field, field = as_field(quo))
}

translate <- function(x, quo, field, by) {
  UseMethod("translate")
}

translate.virgo_window <- function(x, quo, field, by) {
  x$window <- list(
    list(op = x$window$op, field = as_field(quo), as = field))
  x$groupby <- by
  list(x = unclass(x), as = field, field = as_field(quo))
}

translate.virgo_aggregate <- function(x, quo, field, by) {
  x$joinaggregate <- list(
    list(op = x$aggregate, field = as_field(quo), as = field))
  x$groupby <- by
  x$aggregate <- NULL
  list(x = unclass(x), as = field, field = as_field(quo))
}

translate.virgo_bin <- function(x, quo, field, by) {
  list(x = list(bin = unclass(x), field = as_field(quo), as = field),
    as = field, field = as_field(quo))
}

translate.default <- function(x, quo, field, by) {
  list(x = list(calculate = x, as = field), as = field, field = as_field(quo))
}

virgo_trans_env <- function() {
  ops <- c("+", "-", "*", "/", "^", "==", "!=", ">", ">=", "<", "<=")
  fns <- map(ops, function(op) function(e1, e2) {
    if (catch_symbol(e1)) {
      e1 <- paste0("datum.", deparse(substitute(e1)))
    }
    if (catch_symbol(e2)) {
      e2 <- paste0("datum.", deparse(substitute(e2)))
    }
    paste(e1, op, e2)
  })
  new_environment(vec_set_names(fns, ops))
}

eval_trans_mask <- function(quo) {
  data_mask <- new_virgo_mask(list(), virgo_trans_env())
  map(quo, function(x) eval_tidy(x, data_mask))
}

catch_symbol <- function(x) {
  tryCatch(is_symbol(x), error = function(e) TRUE)
}
