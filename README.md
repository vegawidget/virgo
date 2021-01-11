
<!-- README.md is generated from README.Rmd. Please edit that file -->

# virgo

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The **virgo** package enables the creation of interactive graphics for
exploratory data analysis. It is an *idiomatic and opinionated* R
interface to the grammar of graphics implemented by
[**Vega-Lite**](https://vega.github.io/vega-lite/) which defines the
following elements:

  - graphical elements like `mark_point()`, with the `mark_*` family of
    functions
  - interactive elements, such as brushes (using `select_interval()`)
    and sliders (using `input_slider()`), via the `select_*` and
    `input_*` family of functions
  - aesthetic mappings/encodings via `enc()`
  - data transformations via [selections]()
  - plot composition via faceting and concatenation using
    `facet_views()`, `hconcat()` and `vconcat()`

## Installation

You can install the released version of **virgo** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("virgo")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("vegawidget/virgo")
```

## Get started

``` r
library(virgo)
# a more compelling example than mtcars
```

## Learning more

  - [Example gallery]()
  - [Using **virgo** to explore Melbourne’s microclimate]()
  - [Guide to **virgo** for **ggplot2** users]()
  - [Composing plot interactions with selections]()

## Acknowledgements

  - Vega/Vega-Lite developers
  - Ian Lyttle, Hayley Jepson and Alicia Schep for their foundational
    work in the
    [**vegawidget**](https://vegawidget.github.io/vegawidget/) package
