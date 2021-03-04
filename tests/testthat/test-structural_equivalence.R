test_that("mse works", {
  library(igraph)
  g1 <- graph.full(5,directed = FALSE)
  g2 <- graph.full(5,directed = TRUE)
  expect_equal(structural_equivalence(g1),c(1,1,1,1,1))
  expect_error(structural_equivalence(g2))
})
