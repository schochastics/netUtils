# Multiple networks from a single edgelist with a typed attribute

Create a list of igraph objects from an edgelist according to a type
attribute

## Usage

``` r
graph_from_multi_edgelist(
  d,
  from = NULL,
  to = NULL,
  type = NULL,
  weight = NULL,
  directed = FALSE
)
```

## Arguments

- d:

  data frame.

- from:

  column name of sender. If NULL, defaults to first column.

- to:

  column of receiver. If NULL, defaults to second column.

- type:

  type attribute to split the edgelist. If NULL, defaults to third
  column.

- weight:

  optional column name of edge weights. Ignored if NULL.

- directed:

  logical scalar, whether or not to create a directed graph.

## Value

list of igraph objects.

## Author

David Schoch

## Examples

``` r
library(igraph)
d <- data.frame(
    from = rep(c(1, 2, 3), 3), to = rep(c(2, 3, 1), 3),
    type = rep(c("a", "b", "c"), each = 3), weight = 1:9
)
graph_from_multi_edgelist(d, "from", "to", "type", "weight")
#> $a
#> IGRAPH a0b09a0 UNW- 3 3 -- 
#> + attr: name (v/c), weight (e/n), type (e/c)
#> + edges from a0b09a0 (vertex names):
#> [1] 1--2 2--3 1--3
#> 
#> $b
#> IGRAPH ba079be UNW- 3 3 -- 
#> + attr: name (v/c), weight (e/n), type (e/c)
#> + edges from ba079be (vertex names):
#> [1] 1--2 2--3 1--3
#> 
#> $c
#> IGRAPH 8fdafe2 UNW- 3 3 -- 
#> + attr: name (v/c), weight (e/n), type (e/c)
#> + edges from 8fdafe2 (vertex names):
#> [1] 1--2 2--3 1--3
#> 
```
