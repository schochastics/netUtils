# Graph correlation

This function computes the correlation between networks. Implemented
methods expect the graph to be an adjacency matrix or an igraph object.

## Usage

``` r
graph_cor(object1, object2, ...)

# Default S3 method
graph_cor(object1, object2, ...)

# S3 method for class 'igraph'
graph_cor(object1, object2, diag = FALSE, attr = NULL, ...)

# S3 method for class 'matrix'
graph_cor(object1, object2, diag = FALSE, ...)

# S3 method for class 'array'
graph_cor(object1, object2, diag = FALSE, ...)
```

## Arguments

- object1:

  igraph object or adjacency matrix

- object2:

  igraph object or adjacency matrix over the same vertex set as object1

- ...:

  additional arguments (currently unused)

- diag:

  logical. Should the diagonal of the adjacency matrices be included?
  Defaults to `FALSE`, the standard definition of graph correlation.

- attr:

  Either NULL or a character string giving an edge attribute name used
  as edge weights for igraph objects. If NULL, the unweighted adjacency
  matrices are used.

## Value

correlation between graphs
