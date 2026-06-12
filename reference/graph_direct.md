# Direct product of two graphs

Compute the direct product of two graphs

## Usage

``` r
graph_direct(g, h)
```

## Arguments

- g:

  An igraph object

- h:

  An igraph object

## Value

Direct product as igraph object

## Details

See https://en.wikipedia.org/wiki/Tensor_product_of_graphs

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_ring(4)
h <- make_full_graph(2)
graph_direct(g, h)
#> IGRAPH 39f573f UN-- 8 8 -- 
#> + attr: name (v/c)
#> + edges from 39f573f (vertex names):
#> [1] 1-1--2-2 2-1--1-2 2-1--3-2 2-2--3-1 3-1--4-2 3-2--4-1 1-1--4-2 1-2--4-1
```
