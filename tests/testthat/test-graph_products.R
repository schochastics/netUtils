test_that("graph products works", {
  library(igraph)
  g <- make_ring(4)
  h <- make_full_graph(2)
  gh <- graph_cartesian(g,h)
  expect_equal(20,vcount(gh)+ecount(gh))
  hg <- graph_direct(g,h)
  expect_equal(16,vcount(hg)+ecount(hg))
})
