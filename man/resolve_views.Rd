% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiviews.R
\name{resolve_views}
\alias{resolve_views}
\title{Resolve scale and guide for layered and multi-view displays}
\usage{
resolve_views(v, scale = list(), axis = list(), legend = list())
}
\arguments{
\item{v}{A \code{vega()} object.}

\item{scale}{A named list of every channel to define either "shared" or
"independent".}

\item{axis}{A named list of positional channels like \code{x} and \code{y}.}

\item{legend}{A named list of non-positional channels, such as \code{color}/\code{colour},
\code{opacity}, \code{shape}, and \code{size}.}
}
\description{
Resolve scale and guide for layered and multi-view displays
}
