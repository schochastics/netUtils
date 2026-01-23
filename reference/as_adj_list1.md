# Adjacency list

Create adjacency lists from a graph, either for adjacent edges or for
neighboring vertices. This version is faster than the version of igraph
but less general.

## Usage

``` r
as_adj_list1(g)
```

## Arguments

- g:

  An igraph object

## Value

A list of numeric vectors.

## Details

The function does not have a mode parameter and only returns the
adjacency list comparable to as_adj_list(g,mode="all)

## Author

David Schoch

## Examples

``` r
library(igraph)
#> 
#> Attaching package: ‘igraph’
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union
g <- make_ring(10)
as_adj_list1(g)
#> [[1]]
#> [1]  2 10
#> 
#> [[2]]
#> [1] 1 3
#> 
#> [[3]]
#> [1] 2 4
#> 
#> [[4]]
#> [1] 3 5
#> 
#> [[5]]
#> [1] 4 6
#> 
#> [[6]]
#> [1] 5 7
#> 
#> [[7]]
#> [1] 6 8
#> 
#> [[8]]
#> [1] 7 9
#> 
#> [[9]]
#> [1]  8 10
#> 
#> [[10]]
#> [1] 1 9
#> 
```
