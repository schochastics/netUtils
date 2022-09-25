test_that("biggest_component works", {
  library(igraph)
  g <- make_full_graph(4) + make_full_graph(2)
  bg <- biggest_component(g)
  expect_equal(vcount(bg),4L)
})

test_that("delete_isolates works", {
  library(igraph)
  g <- make_full_graph(4) + make_full_graph(1)
  bg <- delete_isolates(g)
  expect_equal(vcount(bg),4L)
})
