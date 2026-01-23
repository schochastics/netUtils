# Discrete core-periphery model

Fits a discrete core-periphery model to a given network

## Usage

``` r
core_periphery(graph, method = "rk1_dc", iter = 500, ...)
```

## Arguments

- graph:

  igraph object

- method:

  algorithm to use (see details)

- iter:

  number of iterations if `method=GA`

- ...:

  other parameters for GA

## Value

list with numeric vector with entries (k1,k2,...ki...) where ki assigns
vertex i to either the core (ki=1) or periphery (ki=0), and the maximal
correlation with an optimal pattern matrix

## Details

The function fits the data to an optimal pattern matrix with a genetic
algorithm (method="GA") or a rank 1 approximation, either with degree
centrality (method="rk1_dc") or eigenvector centrality (method="rk1_ec")
. The rank 1 approximation is computationally far cheaper but also more
experimental. Best is to compare the results from both models.

## References

Borgatti, Stephen P., and Martin G. Everett. "Models of core/periphery
structures." Social networks 21.4 (2000): 375-395.

## Author

David Schoch

## Examples

``` r
set.seed(121)
# split graphs have a perfect core-periphery structure
sg <- split_graph(n = 20, p = 0.3, core = 0.5)
core_periphery(sg)
#> $vec
#>  [1] 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
#> 
#> $corr
#> [1] 1
#> 
```
