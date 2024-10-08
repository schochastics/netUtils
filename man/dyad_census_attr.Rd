% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dyad_census_attr.R
\name{dyad_census_attr}
\alias{dyad_census_attr}
\title{dyad census with node attributes}
\usage{
dyad_census_attr(g, vattr)
}
\arguments{
\item{g}{igraph object. should be a directed graph.}

\item{vattr}{name of vertex attribute to be used.}
}
\value{
dyad census as a data.frame.
}
\description{
dyad census with node attributes
}
\details{
The node attribute should be integers from 1 to max(attr)
}
\examples{
library(igraph)
g <- sample_gnp(10, 0.4, directed = TRUE)
V(g)$attr <- c(rep(1, 5), rep(2, 5))
dyad_census_attr(g, "attr")
}
\author{
David Schoch
}
