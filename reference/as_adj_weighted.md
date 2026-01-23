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
#>       [,1]      [,2]      [,3]     [,4]       [,5]      [,6]       [,7]
#>  [1,]    0 0.0000000 0.0000000 0.000000 0.00000000 0.0000000 0.00000000
#>  [2,]    0 0.0000000 0.0000000 0.000000 0.00000000 0.0000000 0.00000000
#>  [3,]    0 0.0000000 0.0000000 0.000000 0.00000000 0.0000000 0.00000000
#>  [4,]    0 0.0000000 0.0000000 0.000000 0.00000000 0.0000000 0.68855600
#>  [5,]    0 0.0000000 0.0000000 0.000000 0.00000000 0.0000000 0.03123033
#>  [6,]    0 0.0000000 0.0000000 0.000000 0.00000000 0.0000000 0.00000000
#>  [7,]    0 0.0000000 0.0000000 0.688556 0.03123033 0.0000000 0.00000000
#>  [8,]    0 0.0000000 0.2255625 0.000000 0.00000000 0.3008308 0.00000000
#>  [9,]    0 0.0000000 0.0000000 0.000000 0.00000000 0.0000000 0.00000000
#> [10,]    0 0.6364656 0.0000000 0.000000 0.00000000 0.0000000 0.47902455
#>            [,8]      [,9]     [,10]
#>  [1,] 0.0000000 0.0000000 0.0000000
#>  [2,] 0.0000000 0.0000000 0.6364656
#>  [3,] 0.2255625 0.0000000 0.0000000
#>  [4,] 0.0000000 0.0000000 0.0000000
#>  [5,] 0.0000000 0.0000000 0.0000000
#>  [6,] 0.3008308 0.0000000 0.0000000
#>  [7,] 0.0000000 0.0000000 0.4790245
#>  [8,] 0.0000000 0.0000000 0.0000000
#>  [9,] 0.0000000 0.0000000 0.4321713
#> [10,] 0.0000000 0.4321713 0.0000000
```
