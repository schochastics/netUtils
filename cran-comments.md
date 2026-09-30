## Update from 0.8.6 to 1.0.0

This is a major release fixing bugs found in a code review, see NEWS.md for details. Among others:

* fixed incorrect counts in `dyad_census_attr()` and `triad_census_attr()`, and a much faster `triad_census_attr()`
* `sample_lfr()` now uses R's random number generator (it previously used its own, so `set.seed()` had no effect) and reports errors via `Rcpp::stop()`
* compatibility with the upcoming igraph 3.0.0
* dropped the RcppArmadillo dependency, requires igraph >= 2.3.0

## Test environments

* local macOS, R release
* GitHub Actions: macOS-latest (release), windows-latest (release), ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
