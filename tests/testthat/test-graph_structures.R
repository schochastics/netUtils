test_that("as_adj_list1 works", {
    library(igraph)
    l <- list(c(2, 3), c(1), c(1))
    g <- graph_from_adj_list(l, mode = "all")
    expect_equal(l, as_adj_list1(g))
})

test_that("as_adj_weighted works", {
    library(igraph)
    A <- matrix(c(0, 3, 3, 3, 0, 3, 3, 3, 0), 3, 3)
    g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
    expect_equal(as_adj_weighted(g, attr = "weight")[1, 2], 3)
})

test_that("clique_vertex_mat works", {
    library(igraph)
    g <- make_full_graph(3)
    expect_equal(clique_vertex_mat(g), matrix(1, ncol = 3, nrow = 1))
})
