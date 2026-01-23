# Cartesian product of two graphs

Compute the Cartesian product of two graphs

## Usage

``` r
graph_cartesian(g, h)
```

## Arguments

- g:

  An igraph object

- h:

  An igraph object

## Value

Cartesian product as igraph object

## Details

See https://en.wikipedia.org/wiki/Cartesian_product_of_graphs

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_ring(4)
h <- make_full_graph(2)
graph_cartesian(g, h)
#> IGRAPH 5bb2c90 UN-- 8 12 -- 
#> + attr: name (v/c)
#> + edges from 5bb2c90 (vertex names):
#>  [1] 1-1--2-1 1-2--2-2 2-1--3-1 2-2--3-2 3-1--4-1 3-2--4-2 1-1--4-1 1-2--4-2
#>  [9] 1-1--1-2 2-1--2-2 3-1--3-2 4-1--4-2
```
