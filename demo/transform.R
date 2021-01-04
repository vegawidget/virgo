library(dplyr)

movies <- jsonlite::read_json(
  "https://vega.github.io/vega-editor/app/data/movies.json"
  , simplifyVector = TRUE)

as_tibble(movies) %>%
  vega(enc(x = Rotten_Tomatoes_Rating, y = IMDB_Rating)) %>%
  mark_point(enc(colour = vg_bin(IMDB_Rating, step = 3)))

movies %>%
  vega() %>%
  mark_bar(enc(x = vg_argmax(Production_Budget, US_Gross), y = Major_Genre))

as_tibble(movies) %>%
  vega(enc(x = Rotten_Tomatoes_Rating, y = IMDB_Rating)) %>%
  mark_point() %>%
  mark_smooth(enc(colour = Major_Genre))

as_tibble(movies) %>%
  vega(enc(x = Rotten_Tomatoes_Rating, y = IMDB_Rating)) %>%
  mark_point() %>%
  mark_smooth(colour = "firebrick", method = "loess")

as_tibble(movies) %>%
  vega(enc(x = Rotten_Tomatoes_Rating, y = IMDB_Rating)) %>%
  mark_point() %>%
  mark_smooth(colour = "firebrick", selection = !select_interval())

as_tibble(movies) %>%
  vega(enc(x = IMDB_Rating, y = Rotten_Tomatoes_Rating, colour = vg_count())) %>%
  mark_bin2d(bin = list(x = list(maxbins = 60), y = list(maxbins = 40)))
