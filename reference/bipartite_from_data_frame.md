# two-mode network from a data.frame

Create a two-mode network from a data.frame

## Usage

``` r
bipartite_from_data_frame(d, type1, type2, attr = NULL, weighted = TRUE)
```

## Arguments

- d:

  data.frame

- type1:

  column name of mode 1

- type2:

  column name of mode 2

- attr:

  named list of edge attributes

- weighted:

  should a weighted graph be created if multiple edges occur

## Value

two mode network as igraph object

## Author

David Schoch

## Examples

``` r
library(igraph)
edges <- data.frame(mode1 = 1:5, mode2 = letters[1:5])
bipartite_from_data_frame(edges, "mode1", "mode2")
#> IGRAPH 80e9f73 UN-B 10 5 -- 
#> + attr: name (v/c), type (v/l)
#> + edges from 80e9f73 (vertex names):
#> [1] 1--a 2--b 3--c 4--d 5--e
```
