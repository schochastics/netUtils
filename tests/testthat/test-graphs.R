test_that("bipartite_from_data_frame works", {
    library(igraph)
    df <- data.frame(from = c("a", "b", "c"), to = c("A", "A", "B"))
    g <- bipartite_from_data_frame(df, "from", "to")
    expect_equal(vcount(g) + ecount(g), 8)
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
})
