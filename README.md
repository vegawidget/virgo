# virgo


## Big Picture

Goal: to write an interface to the vega-lite JS api in R, make the interface idiomatic and not overly verbose. Tightly integrate with crosstalk (on the client) and shiny reactives. 


## Approaches

1. directly interface with vegawidget. In this case, we are adapting the API to translate into a vega spec at some point.

2. write our own widget that wraps the vega-lite JS API directly.

## API design

Examples are taken from the following notebook:
https://observablehq.com/@uwdata/interaction

```js
const selection = vl.selectSingle();

return vl.markCircle()
  .data(cars)
  .select(selection)
  .encode(
    vl.x().fieldQ('Horsepower'),
    vl.y().fieldQ('Miles_per_Gallon'),
    vl.color().if(selection, vl.fieldO('Cylinders')).value('grey'),
    vl.opacity().if(selection, vl.value(0.8)).value(0.1)
  )
  .render()
```

```r
library(virgo)
selection <- select_single()

vega() %>%
  mark_circle(
    encode = encode(
      x = Horsepower, y = Miles_per_Gallon,
      colour = ifelse(selection, Cylinders, "grey"),
      opacity = ifelse(selection, .8, .1)
    ),
    data = cars,
    select = selection
  )
```

```js
const brush = vl.selectInterval()
  .encodings('x'); // limit selection to x-axis (year) values

// dynamic query histogram
const years = vl.markBar({width: 4})
  .data(movies)
  .select(brush)
  .encode(
    vl.x().year('Release_Date').title('Films by Release Year'),
    vl.y().count().title(null)
  )

// ratings scatter plot
const ratings = vl.markCircle()
  .data(movies)
  .encode(
    vl.x().fieldQ('Rotten_Tomatoes_Rating'),
    vl.y().fieldQ('IMDB_Rating'),
    vl.tooltip().fieldN('Title'),
    vl.opacity().if(brush, vl.value(0.75)).value(0.05)
  )

return vl.vconcat(years, ratings).spacing(5).render();
```

```r
brush <- select_interval(encodings = "x")
years <- vega(movies) %>%
  mark_bar(
    encode = encode(x = year(Release_Date), y = count()),
    width = 4,
    select = brush
  ) %>%
  labs(x = "Films by Release Year")
ratings <- vega(movies) %>%
  mark_circle(
    encode = encode(
      x = Rotten_Tomatoes_Rating, y = IMDB_Rating, tooltip = Title,
      opacity = ifelse(brush, .75, .05)))
years + ratings
```

```js
const brush = vl.selectInterval().encodings('x');
const x = vl.x().fieldT('date').title(null);

const base = vl.markArea()
  .encode(x, vl.y().fieldQ('price'))
  .width(700);

return vl.data(sp500)
  .vconcat(
    base.encode(x.scale({domain: brush})),
    base.select(brush).height(60)
  )
  .render();
```

```r
brush <- select_interval(encodings = "x")

base <- vega(sp500) %>%
  mark_area(encode = encode(x = date, y = price), select = brush)

p2 <- vega(sp500) %>%
  mark_area(encode = encode(x = date, y = price)) %>%
  scale(encoding = "x", domain = brush)

p2 + base
```

```r
vl_chart() %>%
  vl_add_data(url = "https://vega.github.io/vega-editor/app/data/population.json") %>%
  vl_calculate(calculate = "datum.sex == 2 ? 'Female' : 'Male'", 
               as = "gender") %>%
  vl_filter("datum.year == 2000") %>%
  vl_encode(x = "age:O", y = "people:Q", color = "gender:N") %>%
  vl_stack_y("normalize") %>%
  vl_aggregate_y("sum") %>%
  vl_axis_y(title = "population") %>%
  vl_mark_bar()
```

```r
vega(population) %>%
  mutate(sex = ifelse(sex == 2, "Female", "Male")) %>%
  filter(year == 2000) %>%
  mark_bar(
    encode = encode(x = age, y = sum(people), colour = gender),
    position = "fill"
  )
```
