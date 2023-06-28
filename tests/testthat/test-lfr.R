test_that("lfr works", {
  g <- sample_lfr(n = 128, average_degree = 16,
                  max_degree = 16, 
                  min_community = 32, max_community = 32)
  expect_true(all(igraph::degree(g) == 16))
})
