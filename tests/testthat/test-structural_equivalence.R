test_that("mse works", {
    library(igraph)
    g1 <- make_full_graph(5, directed = FALSE)
    g2 <- make_full_graph(5, directed = TRUE)
    expect_equal(structural_equivalence(g1), c(1, 1, 1, 1, 1))
    expect_error(structural_equivalence(g2))
})
