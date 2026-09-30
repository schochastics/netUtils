test_that("core periphery works", {
    g <- igraph::make_full_graph(5, directed = FALSE)
    g <- igraph::add_vertices(g, 5)
    vec1 <- core_periphery(g, method = "rk1_ec")$vec
    expect_equal(vec1, c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))
    vec2 <- core_periphery(g, method = "rk1_dc")$vec
    expect_equal(vec2, c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))
    expect_error(core_periphery(g, method = "l33t"), "method must be one of")
})

test_that("core periphery with deprecated SA runs GA", {
    skip_if_not_installed("GA")
    set.seed(1)
    g <- split_graph(n = 20, p = 0.3, core = 0.5)
    expect_warning(res <- core_periphery(g, method = "SA", iter = 20), "deprecated")
    expect_type(res, "list")
    expect_length(res$vec, 20)
})
