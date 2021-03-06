% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structural_equivalence.R
\name{structural_equivalence}
\alias{structural_equivalence}
\title{Maximal Structural Equivalence}
\usage{
structural_equivalence(g)
}
\arguments{
\item{g}{An igraph object}
}
\value{
vector of equivalence classes
}
\description{
Calculates structural equivalence for an undirected graph
}
\details{
Two nodes u and v are structurally equivalent if they have exactly the same neighbors. The equivalence classes produced with this function are either cliques or empty graphs.
}
\author{
David Schoch
}
