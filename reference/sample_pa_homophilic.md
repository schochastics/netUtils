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

David Schoch

## Examples

``` r
# maximally heterophilic network
sample_pa_homophilic(n = 50, m = 2, minority_fraction = 0.2, h_ab = 1)
#> IGRAPH 00325fa U--- 50 92 -- 
#> + attr: minority (v/l)
#> + edges from 00325fa:
#>  [1]  3-- 5  1-- 5  5-- 6  2-- 6  2-- 7  5-- 7  5-- 8  2-- 8  5-- 9  2-- 9
#> [11]  9--10  6--10  5--11  2--11 11--12  9--12 12--13  5--13 12--14  2--14
#> [21] 10--15 12--15 12--16  5--16  2--17  5--17  5--18  2--18 16--19  8--19
#> [31]  5--20  2--20  2--21  5--21  5--22 12--22  5--23  2--23  2--24  5--24
#> [41]  2--25 19--25 12--26 19--26  5--27  2--27 19--28  5--28 12--29  5--29
#> [51]  5--30  2--30 28--31 23--31 19--32 10--32 10--33 12--33 24--34  9--34
#> [61] 34--35  5--35 27--36 25--36  5--37  2--37  2--38 12--38 34--39  5--39
#> [71]  5--40 12--40 24--41 39--41 10--42  2--42 36--43  5--43  5--44  2--44
#> + ... omitted several edges
# maximally homophilic network
sample_pa_homophilic(n = 50, m = 2, minority_fraction = 0.2, h_ab = 0)
#> IGRAPH 52fc756 U--- 50 92 -- 
#> + attr: minority (v/l)
#> + edges from 52fc756:
#>  [1]  2-- 3  1-- 3  1-- 4  3-- 4  4-- 5  3-- 5  3-- 6  1-- 6  6-- 7  3-- 7
#> [11]  3-- 8  1-- 8  7--10  4--10  3--11  7--11 11--12  3--12 10--13 11--13
#> [21]  3--14  7--14  1--15  7--15 15--16 11--16 15--17  1--17  3--18  5--18
#> [31]  9--20 19--20  3--21 12--21 19--22  9--22  4--23  1--23  4--24  7--24
#> [41] 19--25  9--25 18--26  1--26  8--27  1--27 21--28 26--28  9--29 19--29
#> [51] 15--30  7--30 29--31 25--31 21--32  1--32  1--33  8--33  3--34 24--34
#> [61] 18--35  4--35  1--36  4--36 32--37  3--37 31--38  9--38 16--39 12--39
#> [71] 10--40 14--40 37--41 10--41 16--42  6--42  7--43 15--43 15--44 21--44
#> + ... omitted several edges
```
