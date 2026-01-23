# LFR benchmark graphs

Generates benchmark networks for clustering tasks with a priori known
communities. The algorithm accounts for the heterogeneity in the
distributions of node degrees and of community sizes.

## Usage

``` r
sample_lfr(
  n,
  tau1 = 2,
  tau2 = 1,
  mu = 0.1,
  average_degree,
  max_degree,
  min_community = NULL,
  max_community = NULL,
  on = 0,
  om = 0
)
```

## Arguments

- n:

  Number of nodes in the created graph.

- tau1:

  Power law exponent for the degree distribution of the created graph.
  This value must be strictly greater than one

- tau2:

  Power law exponent for the community size distribution in the created
  graph. This value must be strictly greater than one

- mu:

  Fraction of inter-community edges incident to each node. This value
  must be in the interval 0 to 1.

- average_degree:

  Desired average degree of nodes in the created graph. This value must
  be in the interval 0 to n. Exactly one of this and `min_degree` must
  be specified, otherwise an error is raised

- max_degree:

  Maximum degree of nodes in the created graph. If not specified, this
  is set to n-1.

- min_community:

  Minimum size of communities in the graph. If not specified, this is
  set to `min_degree`

- max_community:

  Maximum size of communities in the graph. If not specified, this is
  set to n, the total number of nodes in the graph.

- on:

  number of overlapping nodes

- om:

  number of memberships of the overlapping nodes

## Value

an igraph object

## Details

code adapted from
<https://github.com/synwalk/synwalk-analysis/tree/master/lfr_generator>

## References

A. Lancichinetti, S. Fortunato, and F. Radicchi.(2008) Benchmark graphs
for testing community detection algorithms. Physical Review E, 78.
arXiv:0805.4770

## Examples

``` r
# Simple Girven-Newman benchmark graphs
g <- sample_lfr(
    n = 128, average_degree = 16,
    max_degree = 16, mu = 0.1,
    min_community = 32, max_community = 32
)
```
