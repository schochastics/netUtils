# split graph

Create a random split graph with a perfect core-periphery structure.

## Usage

``` r
split_graph(n, p, core)
```

## Arguments

- n:

  number of nodes

- p:

  probability of peripheral nodes to connect to the core nodes

- core:

  fraction of nodes in the core

## Value

igraph object

## Author

David Schoch

## Examples

``` r
# split graph with 20 nodes and a core size of 10
split_graph(n = 20, p = 0.4, 0.5)
#> IGRAPH cf4cfa9 U--- 20 79 -- 
#> + attr: core (v/l)
#> + edges from cf4cfa9:
#>  [1]  1-- 2  1-- 3  1-- 4  1-- 5  1-- 6  1-- 7  1-- 8  1-- 9  1--10  1--14
#> [11]  2-- 3  2-- 4  2-- 5  2-- 6  2-- 7  2-- 8  2-- 9  2--10  2--16  2--18
#> [21]  2--19  2--20  3-- 4  3-- 5  3-- 6  3-- 7  3-- 8  3-- 9  3--10  3--11
#> [31]  3--13  3--20  4-- 5  4-- 6  4-- 7  4-- 8  4-- 9  4--10  4--11  4--16
#> [41]  4--19  5-- 6  5-- 7  5-- 8  5-- 9  5--10  5--11  5--13  5--14  5--15
#> [51]  5--18  5--19  5--20  6-- 7  6-- 8  6-- 9  6--10  6--13  6--15  6--18
#> [61]  7-- 8  7-- 9  7--10  7--13  7--16  7--17  7--19  7--20  8-- 9  8--10
#> [71]  8--20  9--10  9--12  9--15  9--18  9--19  9--20 10--12 10--18
```
