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
#> IGRAPH de9dd99 U--- 50 84 -- 
#> + attr: minority (v/l)
#> + edges from de9dd99:
#>  [1]  5-- 9  7-- 9  9--10  1--10  5--11 10--11 11--12  9--12  9--13 11--13
#> [11] 10--14 12--14 14--15  1--15 11--16  9--16 11--17 14--17 14--18  9--18
#> [21] 11--19 14--19 14--20  1--20  9--21 14--21  9--22  1--22  9--23  1--23
#> [31]  9--24  1--24  9--25 14--25 10--26 21--26 21--27 19--27 14--28 11--28
#> [41] 22--29 15--29  1--30 11--30  9--31 11--31 21--32 17--32 11--33 14--33
#> [51] 14--34  9--34 11--35 14--35  9--36 11--36  9--37 14--37  1--38  9--38
#> [61] 35--39 38--39 27--40 26--40 11--41  9--41 17--42 10--42  1--43 14--43
#> [71]  9--44 11--44 26--45  9--45 11--46 14--46  9--47 14--47  1--48 11--48
#> + ... omitted several edges
# maximally homophilic network
sample_pa_homophilic(n = 50, m = 2, minority_fraction = 0.2, h_ab = 0)
#> IGRAPH b3bb204 U--- 50 92 -- 
#> + attr: minority (v/l)
#> + edges from b3bb204:
#>  [1]  1-- 3  2-- 3  3-- 4  2-- 4  4-- 5  2-- 5  3-- 6  1-- 6  3-- 8  6-- 8
#> [11]  6-- 9  3-- 9  3--10  8--10  3--11  8--11  8--12  3--12  3--13  6--13
#> [21]  6--14  2--14  6--15  2--15  3--16  8--16 11--17  2--17 16--18  8--18
#> [31]  7--20 19--20  3--21 17--21  3--22 18--22 17--23 13--23 12--24  2--24
#> [41] 19--25  7--25  3--26 16--26  5--27 10--27  7--28 19--28  3--29 22--29
#> [51] 20--30 19--30 21--31 14--31 15--32  3--32 25--33 28--33  3--34  8--34
#> [61] 27--35 12--35  6--36 32--36 16--37 12--37 20--38 19--38  3--39 15--39
#> [71]  3--40  8--40 34--41 24--41  2--42 26--42 25--43 20--43  3--44 32--44
#> + ... omitted several edges
```
