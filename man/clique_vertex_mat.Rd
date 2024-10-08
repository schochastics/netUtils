% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_structures.R
\name{clique_vertex_mat}
\alias{clique_vertex_mat}
\title{Clique Vertex Matrix}
\usage{
clique_vertex_mat(g)
}
\arguments{
\item{g}{An igraph object}
}
\value{
Numeric matrix
}
\description{
Creates the clique vertex matrix with entries (i,j) equal to one if node j is in clique i
}
\examples{
library(igraph)
g <- sample_gnp(10, 0.2)
clique_vertex_mat(g)
}
\author{
David Schoch
}
