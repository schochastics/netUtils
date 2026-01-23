# Generate random graphs with a given coreness sequence

Similar to
[sample_degseq](https://r.igraph.org/reference/sample_degseq.html) just
with [coreness](https://r.igraph.org/reference/coreness.html)

## Usage

``` r
sample_coreseq(cores)
```

## Arguments

- cores:

  coreness sequence

## Value

igraph object of graph with the same coreness sequence as the input

## Details

The code is an adaption of the python code from
https://github.com/ktvank/Random-Graphs-with-Prescribed-K-Core-Sequences/

## References

Van Koevering, Katherine, Austin R. Benson, and Jon Kleinberg. 2021.
‘Random Graphs with Prescribed K-Core Sequences: A New Null Model for
Network Analysis’. ArXiv:2102.12604.
https://doi.org/10.1145/3442381.3450001.

## Author

David Schoch

## Examples

``` r
library(igraph)
g1 <- make_graph("Zachary")
kcores1 <- coreness(g1)
g2 <- sample_coreseq(kcores1)
kcores2 <- coreness(g2)

# the sorted arrays are the same
all(sort(kcores1) == sort(kcores2))
#> [1] TRUE
```
