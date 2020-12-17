# nocov start
.onLoad <- function(...) {
  s3_register("dplyr::mutate", "virgo_selection")
  s3_register("dplyr::group_by", "virgo_selection")
}
# nocov end
