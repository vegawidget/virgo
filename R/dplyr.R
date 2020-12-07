filter.virgo <- function(.data, ...) {
  predicates <- enquos(...)
  sel <- eval_tidy(predicates[[1]])
  new_trans <- length(.data$transform) + 1
  .data$transform[[new_trans]] <- list(filter = list(
    selection = selection_composition(sel)))
  .data
}

# TODO: should `filter()` just accept selection predicates?
translate_filter_inputs <- function(...) {
  # https://vega.github.io/vega-lite/docs/filter.html
  # cannot think about interactive filter using field predicates
}

# group_by.virgo <- function(.data, ...) {
#   data <- .data$data$values
#   grps <- eval_select(c(...), names(data))
#   new_trans <- length(.data$transform) + 1
#   .data$transform[[new_trans]] <- list(groupby = map_chr(grps, as_string))
#   .data
# }

summarise.virgo <- function(.data, ...) {
  data <- .data$data$values

  quos <- enquos(..., .named = TRUE)
  ops_fn <- map(quos, call_name)
  # TODO: abort if ops_fn are not in virgo_ops()
  ops <- map(quos, function(x) eval_tidy(x, data) %@% "aggregate")
  fields <- map_chr(quos, as_field)
  as <- names(quos)
  lst <- list(op = ops, field = fields, as = as)
  res <- pmap(lst, function(op, field, as, ...) 
    list(op = op, field = field, as = as))
  new_trans <- length(.data$transform) + 1
  .data$transform[[new_trans]] <- list(aggregate = res)
  .data$data$values <- eval_virgo_mask(data, quos, "")
  .data
}

slice_sample.virgo <- function(.data, ..., n = 1000) { # `prop`?
  new_trans <- length(.data$transform) + 1
  .data$transform[[new_trans]] <- list(sample = n)
  .data
}
