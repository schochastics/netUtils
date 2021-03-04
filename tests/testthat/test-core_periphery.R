test_that("core periphery works", {
  library(igraph)
  g <- graph.full(5,directed = FALSE)
  vec1 <- core_periphery(g,method="rk1")$vec
  expect_equal(vec1,c(1,1,1,1,1))
  expect_error(core_periphery(graph_empty,method = "l33t"))
})
