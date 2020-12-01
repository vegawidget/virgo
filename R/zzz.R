# nocov start
.onLoad <- function(...) {
  s3_register("dplyr::slice_sample", "virgo")
  s3_register("dplyr::filter", "virgo")
}
# nocov end
