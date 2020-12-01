filter.virgo <- function(.data, ...) {
  predicates <- enquos(...)
  sel <- eval_tidy(predicates[[1]])
  new_trans <- length(.data$transform) + 1
  .data$transform[[new_trans]] <- list(filter = list(
    selection = selection_composition(sel)))
  .data
}

slice_sample.virgo <- function(.data, ..., n = 1000) { # `prop`?
  new_trans <- length(.data$transform) + 1
  .data$transform[[new_trans]] <- list(sample = n)
  .data
}
