library(dplyr)

## Can't work when composing selections together
# selection <- select_interval() # wo `name` arg
# selection2 <- selection & select_interval() # selection can't be anoymous

# mtcars %>%
#   mutate(cyl = factor(cyl)) %>%
#   vega() %>%
#   mark_points(encoding = enc(x = wt, y = mpg, color = cyl), selection = selection2)

selection <- select_interval(name = "a", encoding = "x")

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_points(
    encoding = enc(x = wt, y = mpg, color = cyl),
    selection = selection)

# cond <- ifelse(selection, "red", "grey")

mtcars %>%
  mutate(cyl = factor(cyl)) %>%
  vega() %>%
  mark_points(
    encoding = enc(x = wt, y = mpg, color = ifelse(selection, cyl, "grey")),
    selection = selection)

encs <- enc(x = wt, y = mpg, color = ifelse(selection, cyl, "grey"))
quo_is_call(encs$color)
quo_is_call(encs$x)
encs2 <- enc(x = wt, y = mpg, color = if (selection) cyl else "grey")
quo_is_call(encs2$color)
call_args(quo_get_expr(encs$color))
call_name(quo_get_expr(encs$color))
eval_bare(call_args(quo_get_expr(encs$color))[[1]])
eval_tidy(call_args(quo_get_expr(encs$color))[[2]], data = mtcars)

# selection <- select_interval()
# mtcars %>%
#   mutate(cyl = factor(cyl)) %>%
#   vega() %>%
#   mark_points(encoding = enc(x = wt, y = mpg, color = cyl),
#     selection = selection(name = "a"))

# Does `selection` associate with layer only?
# Can we do vega() %>% select_interval() %>% mark_*()?
