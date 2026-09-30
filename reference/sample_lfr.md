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
  om = 0,
  verbose = FALSE
)
```

## Arguments

- n:

  Number of nodes in the created graph.

- tau1:

  Power law exponent for the degree distribution of the created graph.
  This value must be at least one.

- tau2:

  Power law exponent for the community size distribution in the created
  graph. This value must be at least one.

- mu:

  Fraction of inter-community edges incident to each node. This value
  must be in the interval 0 to 1.

- average_degree:

  Desired average degree of nodes in the created graph. This value must
  be in the interval (0, n\] and is required.

- max_degree:

  Maximum degree of nodes in the created graph. This value must be in
  the interval (0, n\] and is required.

- min_community:

  Minimum size of communities in the graph. Either both or none of
  `min_community` and `max_community` must be specified. If none are
  specified, the community size range is set automatically to
  `[max(k_min, 3), max_degree]`, where `k_min` is the minimum degree
  implied by `average_degree`, `max_degree` and `tau1`.

- max_community:

  Maximum size of communities in the graph. Must be at least
  `min_community`.

- on:

  number of overlapping nodes (a non-negative integer not larger than
  `n`).

- om:

  number of memberships of the overlapping nodes. Must be at least 2 if
  `on > 0`.

- verbose:

  logical. Should progress messages of the generator be printed?

## Value

an igraph object with two vertex attributes: `membership`, an integer
vector holding the (first) community of each vertex, and `memberships`,
a list holding all communities of each vertex (only overlapping vertices
have more than one).

## Details

code adapted from
<https://github.com/synwalk/synwalk-analysis/tree/master/lfr_generator>.
Random numbers are drawn from R's random number generator, so results
can be reproduced with
[`set.seed()`](https://rdrr.io/r/base/Random.html).

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
