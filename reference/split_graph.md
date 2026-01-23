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
#> IGRAPH a1a357c U--- 20 80 -- 
#> + attr: core (v/l)
#> + edges from a1a357c:
#>  [1]  1-- 2  1-- 3  1-- 4  1-- 5  1-- 6  1-- 7  1-- 8  1-- 9  1--10  1--11
#> [11]  1--12  1--13  1--15  1--16  1--19  2-- 3  2-- 4  2-- 5  2-- 6  2-- 7
#> [21]  2-- 8  2-- 9  2--10  2--11  2--12  2--14  2--16  3-- 4  3-- 5  3-- 6
#> [31]  3-- 7  3-- 8  3-- 9  3--10  3--11  3--13  3--17  3--18  3--19  4-- 5
#> [41]  4-- 6  4-- 7  4-- 8  4-- 9  4--10  4--11  5-- 6  5-- 7  5-- 8  5-- 9
#> [51]  5--10  5--13  5--15  5--19  6-- 7  6-- 8  6-- 9  6--10  6--11  6--12
#> [61]  6--13  6--19  7-- 8  7-- 9  7--10  7--19  7--20  8-- 9  8--10  8--16
#> [71]  8--17  8--20  9--10  9--14  9--18  9--19  9--20 10--13 10--16 10--19
```
