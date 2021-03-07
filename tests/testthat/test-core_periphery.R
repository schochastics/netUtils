test_that("core periphery works", {
  library(igraph)
  g <- graph.full(5,directed = FALSE)
  g <- add.vertices(g,5)
  vec1 <- core_periphery(g,method="rk1_ec")$vec
  expect_equal(vec1,c(1,1,1,1,1,0,0,0,0,0))
  vec2 <- core_periphery(g,method="rk1_dc")$vec
  expect_equal(vec2,c(1,1,1,1,1,0,0,0,0,0))
  expect_error(core_periphery(graph_empty,method = "l33t"))
})
