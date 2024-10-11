
<!-- README.md is generated from README.Rmd. Please edit that file -->

# netUtils <img src="man/figures/logo.png" align="right"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/netUtils)](https://CRAN.R-project.org/package=netUtils)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/netUtils)](https://CRAN.R-project.org/package=netUtils)
[![R-CMD-check](https://github.com/schochastics/netUtils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/schochastics/netUtils/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/schochastics/netUtils/graph/badge.svg)](https://app.codecov.io/gh/schochastics/netUtils)
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

most functions only support igraph objects

**helper/convenience functions**  
`biggest_component()` extracts the biggest connected component of a
network.  
`delete_isolates()` deletes vertices with degree zero.  
`bipartite_from_data_frame()` creates a two mode network from a data
frame.  
`graph_from_multi_edgelist()` creates multiple graphs from a typed
edgelist.  
`clique_vertex_mat()` computes the clique vertex matrix.  
`graph_cartesian()` computes the Cartesian product of two graphs.  
`graph_direct()` computes the direct (or tensor) product of graphs.  
`str()` extends str to work with igraph objects.

**methods**  
`dyad_census_attr()` calculates dyad census with node attributes.  
`triad_census_attr()` calculates triad census with node attributes.  
`core_periphery()` fits a discrete core periphery model.  
`graph_kpartite()` creates a random k-partite network.  
`split_graph()` sample graph with perfect core periphery structure.  
`sample_coreseq()` creates a random graph with given coreness
sequence.  
`sample_pa_homophilic()` creates a preferential attachment graph with
two groups of nodes.  
`sample_lfr()` create LFR benchmark graph for community detection.  
`structural_equivalence()` finds structurally equivalent vertices.  
`reciprocity_cor()` reciprocity as a correlation coefficient.

**methods to use with caution**  
*(this functions should only be used if you know what you are doing)*  
`as_adj_list1()` extracts the adjacency list faster, but less stable,
from igraph objects.  
`as_adj_weighted()` extracts the dense weighted adjacency matrix fast.
