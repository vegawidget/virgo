library(dplyr)

## Can't work when composing selections together
# selection <- select_interval() # wo `name` arg
# selection2 <- selection & select_interval() # selection can't be anoymous

# mtcars %>%
#   mutate(cyl = factor(cyl)) %>%
#   vega() %>%
#   mark_points(encoding = enc(x = wt, y = mpg, color = cyl), selection = selection2)

selection <- select_interval(name = "a")
# cond <- ifelse(selection, "red", "grey")

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_points(
    encoding = enc(x = wt, y = mpg, color = ifelse(selection, cyl, "grey")), 
    selection = selection)

# selection <- select_interval()
# mtcars %>%
#   mutate(cyl = factor(cyl)) %>%
#   vega() %>%
#   mark_points(encoding = enc(x = wt, y = mpg, color = cyl),
#     selection = selection(name = "a"))

# Does `selection` associate with layer only?
# Can we do vega() %>% select_interval() %>% mark_*()?