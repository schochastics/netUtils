# k partite graphs

Create a complete k-partite graph.

## Usage

``` r
graph_kpartite(n = 10, grp = c(5, 5))
```

## Arguments

- n:

  number of nodes

- grp:

  vector of partition sizes. Must sum to `n`.

## Value

igraph object with vertex attribute `type` giving the partition of each
vertex

## Author

David Schoch

## Examples

``` r
# 3-partite graph with equal sized groups
graph_kpartite(n = 15, grp = c(5, 5, 5))
#> IGRAPH f358a1d U--B 15 75 -- 
#> + attr: type (v/n)
#> + edges from f358a1d:
#>  [1]  1-- 6  1-- 7  1-- 8  1-- 9  1--10  1--11  1--12  1--13  1--14  1--15
#> [11]  2-- 6  2-- 7  2-- 8  2-- 9  2--10  2--11  2--12  2--13  2--14  2--15
#> [21]  3-- 6  3-- 7  3-- 8  3-- 9  3--10  3--11  3--12  3--13  3--14  3--15
#> [31]  4-- 6  4-- 7  4-- 8  4-- 9  4--10  4--11  4--12  4--13  4--14  4--15
#> [41]  5-- 6  5-- 7  5-- 8  5-- 9  5--10  5--11  5--12  5--13  5--14  5--15
#> [51]  6--11  6--12  6--13  6--14  6--15  7--11  7--12  7--13  7--14  7--15
#> [61]  8--11  8--12  8--13  8--14  8--15  9--11  9--12  9--13  9--14  9--15
#> [71] 10--11 10--12 10--13 10--14 10--15
```
