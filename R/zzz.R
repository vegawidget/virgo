# nocov start
.onLoad <- function(...) {
  s3_register("dplyr::slice_sample", "virgo")
  s3_register("dplyr::filter", "virgo")
  s3_register("dplyr::summarise", "virgo")
  s3_register("dplyr::mutate", "virgo_selection")
}
# nocov end
