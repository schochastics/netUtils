# netUtils 1.0.0

This release fixes a number of bugs found in a code review. Some fixes change results, notably `dyad_census_attr()`, `triad_census_attr()` (030C orientations), `graph_cor()` (diagonal now excluded by default) and `sample_lfr()` (now uses R's random number generator).

* `dyad_census_attr()` fixed: asymmetric dyads were dropped when a group pair had edges in one direction only, and within-group asymmetric dyads were always reported as 0. Named vertices returned all zeros and graphs without edges errored. Within-group rows now report the total asymmetric count in `asym_ab` and `NA` in `asym_ba`. Multiple edges and loops are ignored.
* `dyad_census_attr()` and `triad_census_attr()` now validate that the vertex attribute holds positive integers without missing values.
* `triad_census_attr()` rewritten: it now runs in roughly O(m * max degree) instead of O(n^3), e.g. seconds instead of hours for thousands of nodes.
* `triad_census_attr()` fixed: the two orientations of cyclic triads (030C) with three distinct attributes were merged into `T030C-abc`; `T030C-cba` is now counted correctly. With more than nine attribute values, names are separated by dots (`T030C-1.2.10`) to avoid ambiguous labels. Multiple edges and loops are ignored.
* dropped the RcppArmadillo dependency.
* `str.igraph()` no longer fails for graphs with a single edge and only appends "..." to truncated attributes.
* `bipartite_from_data_frame()` now handles numeric and factor columns (they were used as vertex ids or factor codes), and merging multiple edges no longer fails with non-numeric edge attributes.
* `structural_equivalence()` now works with multiple edges and no longer needs memory quadratic in the number of vertices.
* `sample_coreseq()` now rejects impossible coreness sequences (a k-core needs at least k + 1 nodes) and invalid input.
* `graph_cartesian()` and `graph_direct()` keep vertex pairs without edges, and are vectorized.
* `as_adj_list1()` now returns all neighbors of directed graphs, as documented (it returned only out-neighbors).
* `graph_cor()` now excludes the diagonal by default, the standard definition of graph correlation. Use `diag = TRUE` for the previous behavior. For igraph objects, the new `attr` argument selects an edge attribute as weights.
* `graph_kpartite()` now uses `igraph::make_full_multipartite()`, errors if the partition sizes do not sum to `n` and stores the partition in the vertex attribute `type`. The documentation now correctly says it creates a complete k-partite graph.
* `graph_from_multi_edgelist()` stores the `weight` column as edge attribute `weight`, so the graphs are weighted, and validates it.
* `split_graph()` validates `core`.
* `as_adj_weighted()`, `as_multi_adj()`, `graph_cor()` and `core_periphery()` work with the upcoming igraph 3.0.0 and keep using unweighted adjacency matrices unless an edge attribute is given.
* removed the internal, unexported `fast_cliques()`.
* requires igraph >= 2.3.0.
* `sample_pa_homophilic()` is faster, validates its input and has working examples; results for a given seed are unchanged.
* `core_periphery(method = "SA")` now actually runs the GA method as announced in its deprecation warning (it returned nothing before).
* `sample_lfr()` now uses R's random number generator, so results are reproducible with `set.seed()` (before, `set.seed()` had no effect).
* `sample_lfr()` with overlapping nodes (`on > 0`) now works (it errored before): `V(g)$membership` holds the first community of each vertex and the new list attribute `V(g)$memberships` holds all communities of each vertex.
* `sample_lfr()` now reports invalid parameter combinations detected by the generator as informative R errors instead of "negative length vectors are not allowed", validates `on`, `om`, `min_community` and `max_community`, and is silent unless the new `verbose = TRUE` is set. It warns if the degree sequence had to be changed.
* `sample_lfr()` can be interrupted and no longer risks endless loops when rewiring links. Unused C++ code of the LFR generator was removed.

# netUtils 0.8.6

* modernized remaining deprecated igraph calls
* internal refactoring to reduce code duplication
* added tests for `str.igraph`

# netUtils 0.8.5

* fix igraph deprecations

# netUtils 0.8.4

* fixed M1mac issues

# netUtils 0.8.3

* added more tests #14
* require igraph 2.0.0

# netUtils 0.8.2

* refactored `sample_lfr()` in C++ (#9)

# netUtils 0.8.1

* fixed a bug that prevented `str.igraph` from working (#10)

# netUtils 0.8.0

* added `reciprocity_cor()`
* fixed wrong str print (#5)
* switched from Simulated Annealing to Genetic Algorithm (#4)
* added more tests
* added `sample_lfr()` (#9)

# netUtils 0.7.0   

* fixed documentation 
* removed unfinished functions
* added examples 

# netUtils 0.6.0.9000

added `sample_pa_homophilic()`

# netUtils 0.5.0.9000

* renamed package to `netUtils`
* added `bipartite_from_data_frame()`
* added `graph_from_multi_edgelist()` and `as_multi_adj()`
* added `structural_equivalence()`
* added `core_periphery()`
* added `sample_coreseq()`
* added tests
* added graph products `graph_cartesian()` and `graph_direct()`
* added fast max clique routine `fast_cliques()`

# igraphUtils 0.1.0

* Added `as_adj_list1()` and `as_adj_weighted()`
* Added `clique_vertex_mat()`
