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

two mode network as igraph object. The vertex attribute `type` is `TRUE`
for vertices from `type1` and `FALSE` for vertices from `type2`.

## Details

Vertex identifiers are converted to character, so numeric and factor
columns are matched by their values. If `weighted = TRUE`, multiple
edges are merged, their count is stored in the edge attribute `weight`,
numeric edge attributes are summed and other edge attributes keep their
first value.

## Author

David Schoch

## Examples

``` r
library(igraph)
edges <- data.frame(mode1 = 1:5, mode2 = letters[1:5])
bipartite_from_data_frame(edges, "mode1", "mode2")
#> IGRAPH b7b7711 UN-B 10 5 -- 
#> + attr: name (v/c), type (v/l)
#> + edges from b7b7711 (vertex names):
#> [1] 1--a 2--b 3--c 4--d 5--e
```
