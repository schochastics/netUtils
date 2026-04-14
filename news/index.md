# Changelog

## netUtils 0.8.5

- fix igraph deprecations

## netUtils 0.8.4

CRAN release: 2026-01-24

- fixed M1mac issues

## netUtils 0.8.3

CRAN release: 2024-10-10

- added more tests
  [\#14](https://github.com/schochastics/netUtils/issues/14)
- require igraph 2.0.0

## netUtils 0.8.2

CRAN release: 2023-06-29

- refactored
  [`sample_lfr()`](https://schochastics.github.io/netUtils/reference/sample_lfr.md)
  in C++ ([\#9](https://github.com/schochastics/netUtils/issues/9))

## netUtils 0.8.1

CRAN release: 2022-12-19

- fixed a bug that prevented `str.igraph` from working
  ([\#10](https://github.com/schochastics/netUtils/issues/10))

## netUtils 0.8.0

CRAN release: 2022-10-08

- added
  [`reciprocity_cor()`](https://schochastics.github.io/netUtils/reference/reciprocity_cor.md)
- fixed wrong str print
  ([\#5](https://github.com/schochastics/netUtils/issues/5))
- switched from Simulated Annealing to Genetic Algorithm
  ([\#4](https://github.com/schochastics/netUtils/issues/4))
- added more tests
- added
  [`sample_lfr()`](https://schochastics.github.io/netUtils/reference/sample_lfr.md)
  ([\#9](https://github.com/schochastics/netUtils/issues/9))

## netUtils 0.7.0

CRAN release: 2022-08-27

- fixed documentation
- removed unfinished functions
- added examples

## netUtils 0.6.0.9000

added
[`sample_pa_homophilic()`](https://schochastics.github.io/netUtils/reference/sample_pa_homophilic.md)

## netUtils 0.5.0.9000

- renamed package to `netUtils`
- added
  [`bipartite_from_data_frame()`](https://schochastics.github.io/netUtils/reference/bipartite_from_data_frame.md)
- added
  [`graph_from_multi_edgelist()`](https://schochastics.github.io/netUtils/reference/graph_from_multi_edgelist.md)
  and
  [`as_multi_adj()`](https://schochastics.github.io/netUtils/reference/as_multi_adj.md)
- added
  [`structural_equivalence()`](https://schochastics.github.io/netUtils/reference/structural_equivalence.md)
- added
  [`core_periphery()`](https://schochastics.github.io/netUtils/reference/core_periphery.md)
- added
  [`sample_coreseq()`](https://schochastics.github.io/netUtils/reference/sample_coreseq.md)
- added tests
- added graph products
  [`graph_cartesian()`](https://schochastics.github.io/netUtils/reference/graph_cartesian.md)
  and
  [`graph_direct()`](https://schochastics.github.io/netUtils/reference/graph_direct.md)
- added fast max clique routine
  [`fast_cliques()`](https://schochastics.github.io/netUtils/reference/fast_cliques.md)
