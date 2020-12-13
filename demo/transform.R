library(dplyr)

movies <- jsonlite::read_json(
  "https://vega.github.io/vega-editor/app/data/movies.json"
  , simplifyVector = TRUE)

movies %>%
  vega() %>%
  mark_bar(enc(x = vg_argmax(Production_Budget, US_Gross), y = Major_Genre))
