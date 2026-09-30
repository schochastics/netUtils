test_that("graph_cor excludes the diagonal by default", {
    A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
    B <- matrix(c(0, 1, 1, 1, 0, 0, 1, 0, 0), 3, 3)
    off <- row(A) != col(A)
    expect_equal(graph_cor(A, B), stats::cor(A[off], B[off]))
    expect_equal(graph_cor(A, B, diag = TRUE), stats::cor(c(A), c(B)))
})

test_that("graph_cor works for igraph objects", {
    set.seed(1)
    g <- igraph::sample_gnp(10, 0.3)
    h <- igraph::sample_gnp(10, 0.3)
    A <- igraph::as_adjacency_matrix(g, sparse = FALSE)
    B <- igraph::as_adjacency_matrix(h, sparse = FALSE)
    expect_equal(graph_cor(g, h), graph_cor(A, B))
    expect_equal(graph_cor(g, g), 1)
    igraph::E(g)$weight <- seq_len(igraph::ecount(g))
    expect_equal(graph_cor(g, h), graph_cor(A, B))
    expect_equal(graph_cor(g, g, attr = "weight"), 1)
})

test_that("graph_cor errors on bad input", {
    expect_error(graph_cor("a", "b"), "don't know how to handle")
    expect_error(graph_cor(matrix(0, 2, 2), matrix(0, 3, 3)), "same dimensions")
})
