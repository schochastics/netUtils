% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphs.R
\name{graph_kpartite}
\alias{graph_kpartite}
\title{k partite graphs}
\usage{
graph_kpartite(n = 10, grp = c(5, 5))
}
\arguments{
\item{n}{number of nodes}

\item{grp}{vector of partition sizes}
}
\value{
igraph object
}
\description{
Create a random k-partite graph.
}
\examples{
# 3-partite graph with equal sized groups
graph_kpartite(n = 15, grp = c(5, 5, 5))
}
\author{
David Schoch
}
