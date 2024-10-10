library(igraph)
test_that("reciprocity_cor works", {
    g <- make_full_graph(5, directed = TRUE)
    expect_equal(reciprocity_cor(g), 0)
})


test_that("reciprocity_cor non igraph error", {
    g <- 42
    expect_error(reciprocity_cor(g))
})

test_that("reciprocity_cor directed error", {
    g <- make_full_graph(5, directed = FALSE)
    expect_error(reciprocity_cor(g))
})
