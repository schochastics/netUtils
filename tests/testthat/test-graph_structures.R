test_that("as_adj_list1 works", {
    library(igraph)
    l <- list(c(2, 3), c(1), c(1))
    g <- graph_from_adj_list(l, mode = "all")
    expect_equal(l, as_adj_list1(g))
})

test_that("as_adj_list1 returns all neighbors of directed graphs", {
    g <- igraph::make_graph(c(1, 2, 3, 1), directed = TRUE)
    expect_equal(as_adj_list1(g), list(c(2L, 3L), 1L, 1L))
    expect_equal(
        as_adj_list1(g),
        lapply(unname(igraph::as_adj_list(g, mode = "all")), as.integer)
    )
})

test_that("as_adj_weighted works", {
    library(igraph)
    A <- matrix(c(0, 3, 3, 3, 0, 3, 3, 3, 0), 3, 3)
    g <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
    expect_equal(as_adj_weighted(g, attr = "weight")[1, 2], 3)
    expect_equal(as_adj_weighted(g)[1, 2], 1)
    expect_error(as_adj_weighted(g, attr = "nope"), "no edge attribute")
})

test_that("as_multi_adj works", {
    g <- igraph::make_ring(3)
    igraph::E(g)$w <- 1:3
    res <- as_multi_adj(list(g, g), attr = "w")
    expect_length(res, 2)
    expect_equal(res[[1]][1, 2], 1)
    expect_equal(res[[1]][1, 3], 3)
    expect_error(as_multi_adj(list(g, 1)), "igraph objects")
})

test_that("clique_vertex_mat works", {
    library(igraph)
    g <- make_full_graph(3)
    expect_equal(clique_vertex_mat(g), matrix(1, ncol = 3, nrow = 1))
})
