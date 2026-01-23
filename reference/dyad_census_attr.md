# dyad census with node attributes

dyad census with node attributes

## Usage

``` r
dyad_census_attr(g, vattr)
```

## Arguments

- g:

  igraph object. should be a directed graph.

- vattr:

  name of vertex attribute to be used.

## Value

dyad census as a data.frame.

## Details

The node attribute should be integers from 1 to max(attr)

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- sample_gnp(10, 0.4, directed = TRUE)
V(g)$attr <- c(rep(1, 5), rep(2, 5))
dyad_census_attr(g, "attr")
#>   from_attr to_attr asym_ab asym_ba sym null
#> 1         1       1       0       0   0   10
#> 2         1       2       7       4   4   10
#> 3         2       2       0       0   2    8
```
