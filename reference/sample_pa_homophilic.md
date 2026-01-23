# Homophilic random graph using BA preferential attachment model

A graph of n nodes is grown by attaching new nodes each with m edges
that are preferentially attached to existing nodes with high degree,
depending on the homophily parameters.

## Usage

``` r
sample_pa_homophilic(
  n,
  m,
  minority_fraction,
  h_ab,
  h_ba = NULL,
  directed = FALSE
)
```

## Arguments

- n:

  number of nodes

- m:

  number of edges a new node is connected to

- minority_fraction:

  fraction of nodes that belong to the minority group

- h_ab:

  probability to connect a node from group a with groub b

- h_ba:

  probability to connect a node from group b with groub a. If NULL, h_ab
  is used.

- directed:

  should a directed network be created

## Value

igraph object

## Details

The code is an adaption of the python code from
https://github.com/gesiscss/HomophilicNtwMinorities/

## References

Karimi, F., Génois, M., Wagner, C., Singer, P., & Strohmaier, M. (2018).
Homophily influences ranking of minorities in social networks.
Scientific reports, 8(1), 1-12.
(https://www.nature.com/articles/s41598-018-29405-7)

Espín-Noboa, L., Wagner, C., Strohmaier, M., & Karimi, F. (2022).
Inequality and inequity in network-based ranking and recommendation
algorithms. Scientific reports, 12(1), 1-14.
(https://www.nature.com/articles/s41598-022-05434-1)

## Author

David Schoch \#maximally heterophilic network sample_pa_homophilic(n =
50, m = 2,minority_fraction = 0.2,h_ab = 1) \#maximally homophilic
network sample_pa_homophilic(n = 50, m = 2,minority_fraction = 0.2,h_ab
= 0)
