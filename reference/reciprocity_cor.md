# Reciprocity correlation coefficient

Reciprocity correlation coefficient

## Usage

``` r
reciprocity_cor(g)
```

## Arguments

- g:

  igraph object. should be a directed graph

## Value

Reciprocity as a correlation

## Details

The usual definition of reciprocity has some defects. It cannot tell the
relative difference of reciprocity compared with purely random network
with the same number of vertices and edges. The useful information from
reciprocity is not the value itself, but whether mutual links occur more
or less often than expected by chance.

To overcome this issue, reciprocity can be defined as the correlation
coefficient between the entries of the adjacency matrix of a directed
graph: \$\$ \frac{\sum\_{i\neq j} (a\_{ij} - a')((a\_{ji} -
a')}{\sum\_{i\neq j} (a\_{ij} - a')^2} \$\$ where a' is the density of
g.

This definition gives an absolute quantity which directly allows one to
distinguish between reciprocal (\>0) and antireciprocal (\< 0) networks,
with mutual links occurring more and less often than random
respectively.

## References

Diego Garlaschelli; Loffredo, Maria I. (2004). "Patterns of Link
Reciprocity in Directed Networks". Physical Review Letters. American
Physical Society. 93 (26): 268701

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- sample_gnp(20, p = 0.3, directed = TRUE)
reciprocity(g)
#> [1] 0.3779528
reciprocity_cor(g)
#> [1] 0.06569979
```
