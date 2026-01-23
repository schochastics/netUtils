# triad census with node attributes

triad census with node attributes

## Usage

``` r
triad_census_attr(g, vattr)
```

## Arguments

- g:

  igraph object. should be a directed graph

- vattr:

  name of vertex attribute to be used

## Value

triad census with node attributes

## Details

The node attribute should be integers from 1 to max(attr). The output is
a named vector where the names are of the form Txxx-abc, where xxx
corresponds to the standard triad census notation and "abc" are the
attributes of the involved nodes.

The implemented algorithm is comparable to the algorithm in Lienert et
al.

## References

Lienert, J., Koehly, L., Reed-Tsochas, F., & Marcum, C. S. (2019). An
efficient counting method for the colored triad census. Social Networks,
58, 136-142.

## Author

David Schoch

## Examples

``` r
library(igraph)
set.seed(112)
g <- sample_gnp(20, p = 0.3, directed = TRUE)
# add a vertex attribute
V(g)$type <- rep(1:2, each = 10)
triad_census_attr(g, "type")
#>  T003-111  T003-112  T003-122  T003-222  T012-111  T012-121  T012-112  T012-122 
#>         8        33        28         7        32        40        31        19 
#>  T012-211  T012-221  T012-212  T012-222 T021D-111 T021D-211 T021D-112 T021D-212 
#>        27        41        25        26         9        19        19        21 
#> T021D-122 T021D-222  T102-111  T102-112  T102-122  T102-211  T102-212  T102-222 
#>         7        10        11        18        16         5        19        10 
#> T021C-111 T021C-211 T021C-121 T021C-221 T021C-112 T021C-212 T021C-122 T021C-222 
#>        17        23        29        17        19         7        24        10 
#> T111U-111 T111U-121 T111U-112 T111U-122 T111U-211 T111U-221 T111U-212 T111U-222 
#>         9        16         7        21         5        13        10         6 
#> T021U-111 T021U-112 T021U-122 T021U-211 T021U-212 T021U-222 T030T-111 T030T-121 
#>        11        19        13         3        14         7        11        11 
#> T030T-112 T030T-122 T030T-211 T030T-221 T030T-212 T030T-222 T120U-111 T120U-112 
#>        11        13        10        14         8         5         1         8 
#> T120U-122 T120U-211 T120U-212 T120U-222 T111D-111 T111D-121 T111D-112 T111D-122 
#>         6         0         4         4         4        12         8        13 
#> T111D-211 T111D-221 T111D-212 T111D-222  T201-111  T201-112  T201-121  T201-122 
#>        14        20        10        15         0         5         3         5 
#>  T201-221  T201-222 T030C-111 T030C-112 T030C-122 T030C-222 T120C-111 T120C-121 
#>         3         3         2        12        14         3         3         8 
#> T120C-211 T120C-221 T120C-112 T120C-122 T120C-212 T120C-222 T120D-111 T120D-112 
#>         7         5         5         7         7         6         0         9 
#> T120D-211 T120D-212 T120D-122 T120D-222  T210-111  T210-121  T210-211  T210-221 
#>         1         9         4         1         2         8         3         5 
#>  T210-112  T210-122  T210-212  T210-222  T300-111  T300-112  T300-122  T300-222 
#>         1         3         5         5         0         1         0         2 
```
