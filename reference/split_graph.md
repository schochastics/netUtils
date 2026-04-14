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
#> IGRAPH b0d197a U--- 20 78 -- 
#> + attr: core (v/l)
#> + edges from b0d197a:
#>  [1]  1-- 2  1-- 3  1-- 4  1-- 5  1-- 6  1-- 7  1-- 8  1-- 9  1--10  1--13
#> [11]  1--14  1--15  1--16  2-- 3  2-- 4  2-- 5  2-- 6  2-- 7  2-- 8  2-- 9
#> [21]  2--10  2--11  3-- 4  3-- 5  3-- 6  3-- 7  3-- 8  3-- 9  3--10  3--19
#> [31]  3--20  4-- 5  4-- 6  4-- 7  4-- 8  4-- 9  4--10  4--13  4--17  5-- 6
#> [41]  5-- 7  5-- 8  5-- 9  5--10  5--16  5--19  6-- 7  6-- 8  6-- 9  6--10
#> [51]  6--12  6--13  6--14  6--15  6--17  6--18  7-- 8  7-- 9  7--10  7--13
#> [61]  7--14  7--16  7--18  8-- 9  8--10  8--12  8--13  8--15  8--19  8--20
#> [71]  9--10  9--11  9--12  9--13 10--11 10--12 10--15 10--17
```
