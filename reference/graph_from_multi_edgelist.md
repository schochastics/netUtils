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

  optional column name of edge weights. The column is stored as edge
  attribute `weight`. Ignored if NULL.

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
#> IGRAPH 808f0b2 UNW- 3 3 -- 
#> + attr: name (v/c), weight (e/n), type (e/c)
#> + edges from 808f0b2 (vertex names):
#> [1] 1--2 2--3 1--3
#> 
#> $b
#> IGRAPH bb0df08 UNW- 3 3 -- 
#> + attr: name (v/c), weight (e/n), type (e/c)
#> + edges from bb0df08 (vertex names):
#> [1] 1--2 2--3 1--3
#> 
#> $c
#> IGRAPH 36f4254 UNW- 3 3 -- 
#> + attr: name (v/c), weight (e/n), type (e/c)
#> + edges from 36f4254 (vertex names):
#> [1] 1--2 2--3 1--3
#> 
```
