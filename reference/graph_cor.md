# Graph correlation

This function computes the correlation between networks. Implemented
methods expect the graph to be an adjacency matrix, an igraph, or a
network object.

## Usage

``` r
graph_cor(object1, object2)

# Default S3 method
graph_cor(object1, object2)

# S3 method for class 'igraph'
graph_cor(object1, object2, ...)

# S3 method for class 'matrix'
graph_cor(object1, object2)

# S3 method for class 'array'
graph_cor(object1, object2)
```

## Arguments

- object1:

  igraph object or adjacency matrix

- object2:

  igraph object or adjacency matrix over the same vertex set as object1

- ...:

  additional arguments

## Value

correlation between graphs
