# Clique Vertex Matrix

Creates the clique vertex matrix with entries (i,j) equal to one if node
j is in clique i

## Usage

``` r
clique_vertex_mat(g)
```

## Arguments

- g:

  An igraph object

## Value

Numeric matrix

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- sample_gnp(10, 0.2)
clique_vertex_mat(g)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]    0    0    0    0    0    1    0    0    0     0
#>  [2,]    0    1    0    0    0    0    1    0    0     0
#>  [3,]    1    0    0    1    0    0    0    0    0     0
#>  [4,]    0    0    0    0    1    0    0    1    0     0
#>  [5,]    1    0    1    0    0    0    0    0    0     0
#>  [6,]    0    0    0    0    0    0    1    0    1     0
#>  [7,]    0    0    0    0    0    0    1    1    0     0
#>  [8,]    0    0    1    0    0    0    1    0    0     0
#>  [9,]    0    0    0    0    0    0    0    1    0     1
#> [10,]    0    0    1    0    0    0    0    0    0     1
```
