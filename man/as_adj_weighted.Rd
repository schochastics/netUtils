% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_structures.R
\name{as_adj_weighted}
\alias{as_adj_weighted}
\title{weighted dense adjacency matrix}
\usage{
as_adj_weighted(g, attr = NULL)
}
\arguments{
\item{g}{An igraph object}

\item{attr}{Either NULL or a character string giving an edge attribute name. If NULL a traditional adjacency matrix is returned. If not NULL then the values of the given edge attribute are included in the adjacency matrix.}
}
\value{
Numeric matrix
}
\description{
returns the weighted adjacency matrix in dense format
}
\details{
This method is faster than as_adj from igraph if you need the weighted adjacency matrix in dense format
}
\examples{
library(igraph)
g <- sample_gnp(10, 0.2)
E(g)$weight <- runif(ecount(g))
as_adj_weighted(g, attr = "weight")
}
\author{
David Schoch
}
