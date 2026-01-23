# Maximal Structural Equivalence

Calculates structural equivalence for an undirected graph

## Usage

``` r
structural_equivalence(g)
```

## Arguments

- g:

  An igraph object

## Value

vector of equivalence classes

## Details

Two nodes u and v are structurally equivalent if they have exactly the
same neighbors. The equivalence classes produced with this function are
either cliques or empty graphs.

## Author

David Schoch
