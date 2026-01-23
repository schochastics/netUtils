# Find Cliques, maximal or not, fast

Enumerates all (maximal) cliques using MACE. Can be faster than igraph
in some circumstances

## Usage

``` r
fast_cliques(g, what = "M", min = NULL, max = NULL, outfile = NA)
```

## Arguments

- g:

  An igraph object

- what:

  either "M" for maximal cliques or "C" for all cliques

- min:

  Numeric constant, lower limit on the size of the cliques to find. NULL
  means no limit, ie. it is the same as 0

- max:

  Numeric constant, upper limit on the size of the cliques to find. NULL
  means no limit

- outfile:

  character. If not NA, cliques are written to file

## Value

a list containing numeric vectors of vertex ids. Each list element is a
clique. If outfile!=NA, the output is written to the specified file

## Details

C Code downloaded from http://research.nii.ac.jp/~uno/codes.htm.
Download the code and run make and then point an environment variable
called MACE_PATH to the binary. See
http://research.nii.ac.jp/~uno/code/mace.html for more details. MACE is
faster than igraph for dense graphs.

## References

Kazuhisa Makino, Takeaki Uno, "New Algorithms for Enumerating All
Maximal Cliques", Lecture Notes in Computer Science 3111 (Proceedings of
SWAT 2004), Springer, pp.260-272, 2004

## Author

David Schoch
