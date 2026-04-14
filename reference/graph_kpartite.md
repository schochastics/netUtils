# k partite graphs

Create a random k-partite graph.

## Usage

``` r
graph_kpartite(n = 10, grp = c(5, 5))
```

## Arguments

- n:

  number of nodes

- grp:

  vector of partition sizes

## Value

igraph object

## Author

David Schoch

## Examples

``` r
# 3-partite graph with equal sized groups
graph_kpartite(n = 15, grp = c(5, 5, 5))
#> IGRAPH c2e487b U--- 15 75 -- 
#> + edges from c2e487b:
#>  [1]  1-- 6  2-- 6  3-- 6  4-- 6  5-- 6  1-- 7  2-- 7  3-- 7  4-- 7  5-- 7
#> [11]  1-- 8  2-- 8  3-- 8  4-- 8  5-- 8  1-- 9  2-- 9  3-- 9  4-- 9  5-- 9
#> [21]  1--10  2--10  3--10  4--10  5--10  1--11  2--11  3--11  4--11  5--11
#> [31]  1--12  2--12  3--12  4--12  5--12  1--13  2--13  3--13  4--13  5--13
#> [41]  1--14  2--14  3--14  4--14  5--14  1--15  2--15  3--15  4--15  5--15
#> [51]  6--11  7--11  8--11  9--11 10--11  6--12  7--12  8--12  9--12 10--12
#> [61]  6--13  7--13  8--13  9--13 10--13  6--14  7--14  8--14  9--14 10--14
#> [71]  6--15  7--15  8--15  9--15 10--15
```
