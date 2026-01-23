# Convert a list of graphs to an adjacency matrices

Convenience function that turns a list of igraph objects into adjacency
matrices.

## Usage

``` r
as_multi_adj(g_lst, attr = NULL, sparse = FALSE)
```

## Arguments

- g_lst:

  A list of igraph object

- attr:

  Either NULL or a character string giving an edge attribute name. If
  NULL a binary adjacency matrix is returned.

- sparse:

  Logical scalar, whether to create a sparse matrix. The 'Matrix'
  package must be installed for creating sparse matrices.

## Value

List of numeric matrices

## Author

David Schoch
