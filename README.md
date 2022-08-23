
<!-- README.md is generated from README.Rmd. Please edit that file -->

# netUtils <img src="man/figures/logo.png" align="right"/>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/schochastics/netUtils/branch/master/graph/badge.svg)](https://codecov.io/gh/schochastics/netUtils?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/netUtils)](https://CRAN.R-project.org/package=netUtils)
<!-- badges: end -->

netUtils is a collection of tools for network analysis that may not
deserve a package on their own and/or are missing from other network
packages.

## Installation

You can install the development version of netUtils with:

``` r
# install.packages("remotes")
remotes::install_github("schochastics/netUtils")
```

## Functions

*(The functions listed below are just a sample of the available
methods)*  
most functions only support igraph objects

**helper/convenience functions**  
`biggest_component()` extract the biggest connected component of a
network.  
`delete_isolates()` delete vertices with degree zero.  
`bipartite_from_data_frame()` create a two mode network from a data
frame.  
`clique_vertex_mat()` compute the clique vertex matrix  
`graph_cartesian()` computes the Cartesian product of two graphs  
`graph_direct()` computes the direct (or tensor) product of graphs

**methods**  
`graph_kpartite()` create a random k-partite network. ![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)  
<!-- `dyad_census_attr()` calculate dyad census with vertex attributes. ![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)   -->
`triad_census_attr()` calculate triad census with vertex attributes.
![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)  
`structural_equivalence()` finds structurally equivalent vertices.
![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)  
`core_periphery()` to fit a discrete core periphery model. ![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-experimental-orange.svg)  
`sample_coreseq()` creates a random graph with given coreness sequence.
![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)  
`fast_clique()` to calculate cliques with MACE (sometimes faster than
igraph) ![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)  
`sample_pa_homophilic()` to create a preferential attachment graph with
two groups of nodes ![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
