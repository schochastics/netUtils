# weighted dense adjacency matrix

returns the weighted adjacency matrix in dense format

## Usage

``` r
as_adj_weighted(g, attr = NULL)
```

## Arguments

- g:

  An igraph object

- attr:

  Either NULL or a character string giving an edge attribute name. If
  NULL a traditional adjacency matrix is returned. If not NULL then the
  values of the given edge attribute are included in the adjacency
  matrix.

## Value

Numeric matrix

## Details

This method is faster than as_adj from igraph if you need the weighted
adjacency matrix in dense format

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- sample_gnp(10, 0.2)
E(g)$weight <- runif(ecount(g))
as_adj_weighted(g, attr = "weight")
#>       [,1]      [,2]      [,3]      [,4]      [,5]      [,6] [,7] [,8]
#>  [1,]    0 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000    0    0
#>  [2,]    0 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000    0    0
#>  [3,]    0 0.0000000 0.0000000 0.0000000 0.0000000 0.2898923    0    0
#>  [4,]    0 0.0000000 0.0000000 0.0000000 0.0000000 0.6783804    0    0
#>  [5,]    0 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000    0    0
#>  [6,]    0 0.0000000 0.2898923 0.6783804 0.0000000 0.0000000    0    0
#>  [7,]    0 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000    0    0
#>  [8,]    0 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000    0    0
#>  [9,]    0 0.0000000 0.0000000 0.0000000 0.7353196 0.1959567    0    0
#> [10,]    0 0.9805397 0.0000000 0.0000000 0.7415215 0.0000000    0    0
#>            [,9]     [,10]
#>  [1,] 0.0000000 0.0000000
#>  [2,] 0.0000000 0.9805397
#>  [3,] 0.0000000 0.0000000
#>  [4,] 0.0000000 0.0000000
#>  [5,] 0.7353196 0.7415215
#>  [6,] 0.1959567 0.0000000
#>  [7,] 0.0000000 0.0000000
#>  [8,] 0.0000000 0.0000000
#>  [9,] 0.0000000 0.0000000
#> [10,] 0.0000000 0.0000000
```
