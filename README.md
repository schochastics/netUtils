
<!-- README.md is generated from README.Rmd. Please edit that file -->

# igraphUtils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

igraphUtils is a collection of network analytic functions that may not
deserve a package on their own.

## Installation

You can install the development version of igraphUtils with:

``` r
# install.packages("remotes")
remotes::install_github("schochastics/igraphUtils")
```

## Functions

**helpers**  
`biggest_component()` extract the biggest connected component of a
network.  
`delete_isolates()` delete vertices with degree zero.

**new algorithms**  
`triad_census_attr()` calculate triad census with vertex attributes.  
`structural_equivalence()` finds structurally equivalent vertices.
