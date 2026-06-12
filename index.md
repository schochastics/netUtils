# netUtils

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
[`biggest_component()`](https://schochastics.github.io/netUtils/reference/helpers.md)
extracts the biggest connected component of a network.  
[`delete_isolates()`](https://schochastics.github.io/netUtils/reference/helpers.md)
deletes vertices with degree zero.  
[`bipartite_from_data_frame()`](https://schochastics.github.io/netUtils/reference/bipartite_from_data_frame.md)
creates a two mode network from a data frame.  
[`graph_from_multi_edgelist()`](https://schochastics.github.io/netUtils/reference/graph_from_multi_edgelist.md)
creates multiple graphs from a typed edgelist.  
[`clique_vertex_mat()`](https://schochastics.github.io/netUtils/reference/clique_vertex_mat.md)
computes the clique vertex matrix.  
[`graph_cartesian()`](https://schochastics.github.io/netUtils/reference/graph_cartesian.md)
computes the Cartesian product of two graphs.  
[`graph_direct()`](https://schochastics.github.io/netUtils/reference/graph_direct.md)
computes the direct (or tensor) product of graphs.  
[`str()`](https://rdrr.io/r/utils/str.html) extends str to work with
igraph objects.

**methods**  
[`dyad_census_attr()`](https://schochastics.github.io/netUtils/reference/dyad_census_attr.md)
calculates dyad census with node attributes.  
[`triad_census_attr()`](https://schochastics.github.io/netUtils/reference/triad_census_attr.md)
calculates triad census with node attributes.  
[`core_periphery()`](https://schochastics.github.io/netUtils/reference/core_periphery.md)
fits a discrete core periphery model.  
[`graph_kpartite()`](https://schochastics.github.io/netUtils/reference/graph_kpartite.md)
creates a random k-partite network.  
[`split_graph()`](https://schochastics.github.io/netUtils/reference/split_graph.md)
sample graph with perfect core periphery structure.  
[`sample_coreseq()`](https://schochastics.github.io/netUtils/reference/sample_coreseq.md)
creates a random graph with given coreness sequence.  
[`sample_pa_homophilic()`](https://schochastics.github.io/netUtils/reference/sample_pa_homophilic.md)
creates a preferential attachment graph with two groups of nodes.  
[`sample_lfr()`](https://schochastics.github.io/netUtils/reference/sample_lfr.md)
create LFR benchmark graph for community detection.  
[`structural_equivalence()`](https://schochastics.github.io/netUtils/reference/structural_equivalence.md)
finds structurally equivalent vertices.  
[`reciprocity_cor()`](https://schochastics.github.io/netUtils/reference/reciprocity_cor.md)
reciprocity as a correlation coefficient.

**methods to use with caution**  
*(this functions should only be used if you know what you are doing)*  
[`as_adj_list1()`](https://schochastics.github.io/netUtils/reference/as_adj_list1.md)
extracts the adjacency list faster, but less stable, from igraph
objects.  
[`as_adj_weighted()`](https://schochastics.github.io/netUtils/reference/as_adj_weighted.md)
extracts the dense weighted adjacency matrix fast.
