library(dplyr)

movies <- jsonlite::read_json(
  "https://vega.github.io/vega-editor/app/data/movies.json"
  , simplifyVector = TRUE)

movies %>%
  vega() %>%
  mark_bar(enc(x = vg_argmax(Production_Budget, US_Gross), y = Major_Genre))

# FIXME: not sure why the line exceeds the limit, but not for loess
as_tibble(movies) %>%
  vega(enc(x = Rotten_Tomatoes_Rating, y = IMDB_Rating)) %>%
  mark_point() %>%
  mark_smooth(colour = "firebrick")

as_tibble(movies) %>%
  vega(enc(x = Rotten_Tomatoes_Rating, y = IMDB_Rating)) %>%
  mark_point() %>%
  mark_smooth(colour = "firebrick", method = "loess")

as_tibble(movies) %>%
  vega(enc(x = Rotten_Tomatoes_Rating, y = IMDB_Rating)) %>%
  mark_point() %>%
  mark_smooth(colour = "firebrick", selection = !select_interval())
