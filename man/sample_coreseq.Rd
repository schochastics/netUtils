% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_kcores.R
\name{sample_coreseq}
\alias{sample_coreseq}
\title{Generate random graphs with a given coreness sequence}
\usage{
sample_coreseq(cores)
}
\arguments{
\item{cores}{coreness sequence}
}
\value{
igraph object of graph with the same coreness sequence as the input
}
\description{
Similar to \link[igraph]{sample_degseq} just with \link[igraph]{coreness}
}
\details{
The code is an adaption of the python code from here: adopted from https://github.com/ktvank/Random-Graphs-with-Prescribed-K-Core-Sequences/
}
\references{
Van Koevering, Katherine, Austin R. Benson, and Jon Kleinberg. 2021. ‘Random Graphs with Prescribed K-Core Sequences: A New Null Model for Network Analysis’. ArXiv:2102.12604. https://doi.org/10.1145/3442381.3450001.
}
\author{
David Schoch
}
