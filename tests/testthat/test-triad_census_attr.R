test_that("triad census attr works", {
    library(igraph)
    gundir <- make_full_graph(3, directed = FALSE)
    gnoatt <- make_full_graph(3, directed = TRUE)
    gchatt <- make_full_graph(3, directed = TRUE)
    vertex_attr(gchatt, "test") <- c("a", "b", "c")
    gtriad <- make_full_graph(3, directed = TRUE)
    vertex_attr(gtriad, "test") <- 1:3
    expect_error(triad_census_attr(gundir))
    expect_error(triad_census_attr(gnoatt))
    expect_error(triad_census_attr(gchatt, "test"))
    expect_true(triad_census_attr(gtriad, "test")["T300-123"] == 1)
})
