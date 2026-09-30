test_that("bipartite_from_data_frame works", {
    library(igraph)
    df <- data.frame(from = c("a", "b", "c"), to = c("A", "A", "B"))
    g <- bipartite_from_data_frame(df, "from", "to")
    expect_equal(vcount(g) + ecount(g), 8)
})

test_that("bipartite_from_data_frame handles numeric and factor columns", {
    g <- bipartite_from_data_frame(data.frame(a = c(10, 20), b = c(30, 40)), "a", "b")
    expect_equal(igraph::V(g)$name, c("10", "20", "30", "40"))
    expect_equal(igraph::as_edgelist(g), cbind(c("10", "20"), c("30", "40")))
    expect_equal(igraph::V(g)$type, c(TRUE, TRUE, FALSE, FALSE))

    g <- bipartite_from_data_frame(
        data.frame(a = factor(c("x", "y")), b = factor(c("u", "v"))), "a", "b"
    )
    expect_equal(igraph::as_edgelist(g), cbind(c("x", "y"), c("u", "v")))
})

test_that("bipartite_from_data_frame merges multiple edges with attributes", {
    df <- data.frame(from = c("a", "a", "b"), to = c("A", "A", "B"))
    g <- bipartite_from_data_frame(df, "from", "to",
        attr = list(w = c(1, 2, 3), label = c("p", "q", "r"))
    )
    expect_equal(igraph::ecount(g), 2)
    expect_equal(igraph::E(g)$weight, c(2, 1))
    expect_equal(igraph::E(g)$w, c(3, 3))
    expect_equal(igraph::E(g)$label, c("p", "r"))
})

test_that("bipartite_from_data_frame type1 error", {
    library(igraph)
    df <- data.frame(from = c("a", "b", "c"), to = c("A", "A", "B"))
    expect_error(bipartite_from_data_frame(df, "froom", "to"))
})

test_that("bipartite_from_data_frame overlap error", {
    library(igraph)
    df <- data.frame(from = c("a", "b", "c"), to = c("a", "A", "B"))
    expect_error(bipartite_from_data_frame(df, "from", "to"))
})

test_that("graph_from_multi_edgelist works", {
    library(igraph)
    d <- data.frame(
        from = rep(c(1, 2, 3), 3), to = rep(c(2, 3, 1), 3),
        type = rep(c("a", "b", "c"), each = 3), weight = 1:9
    )
    g <- graph_from_multi_edgelist(d, "from", "to", "type", "weight")
    expect_equal(length(g), 3)
})

test_that("graph_kpartite works", {
    library(igraph)
    g <- graph_kpartite(n = 15, grp = c(5, 5, 5))
    expect_equal(vcount(g) + ecount(g), 15 + 75)
    expect_equal(V(g)$type, rep(1:3, each = 5))
    expect_length(graph_attr_names(g), 0)
    expect_error(graph_kpartite(n = 10, grp = c(2, 2)), "sum to n")
})

test_that("graph_from_multi_edgelist stores weights and validates input", {
    d <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1), type = c("a", "a", "b"), strength = 1:3)
    g <- graph_from_multi_edgelist(d, "from", "to", "type", "strength")
    expect_true(igraph::is_weighted(g[["a"]]))
    expect_equal(igraph::E(g[["a"]])$weight, 1:2)
    expect_error(graph_from_multi_edgelist(d, "from", "to", "type", "w"), "not a valid column")
    expect_error(graph_from_multi_edgelist(d[, 1:2]), "three columns")
})

test_that("split_graph works", {
    set.seed(1)
    g <- split_graph(n = 10, p = 0.5, core = 0.3)
    expect_equal(igraph::V(g)$core, rep(c(TRUE, FALSE), c(3, 7)))
    expect_error(split_graph(n = 10, p = 0.5, core = 0), "core must be")
})
