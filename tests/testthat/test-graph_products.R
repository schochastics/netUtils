test_that("graph products works", {
    g <- igraph::make_ring(4)
    h <- igraph::make_full_graph(2)
    gh <- graph_cartesian(g, h)
    expect_equal(20, igraph::vcount(gh) + igraph::ecount(gh))
    hg <- graph_direct(g, h)
    expect_equal(16, igraph::vcount(hg) + igraph::ecount(hg))
})

test_that("cartesian product has the expected structure", {
    # C4 x K2 is the cube graph
    gh <- graph_cartesian(igraph::make_ring(4), igraph::make_full_graph(2))
    expect_true(igraph::isomorphic(gh, igraph::make_lattice(c(2, 2, 2))))
    # P2 x P3 is the 2x3 grid
    gh <- graph_cartesian(igraph::make_ring(2, circular = FALSE), igraph::make_ring(3, circular = FALSE))
    expect_true(igraph::isomorphic(gh, igraph::make_lattice(c(2, 3))))
    expect_true(all(c("1-1", "2-3") %in% igraph::V(gh)$name))
})

test_that("graph products keep vertices without edges", {
    g <- igraph::make_graph(c(1, 2), n = 3, directed = FALSE)
    h <- igraph::make_graph(c(1, 2), n = 3, directed = FALSE)
    expect_equal(igraph::vcount(graph_cartesian(g, h)), 9)
    expect_equal(igraph::ecount(graph_cartesian(g, h)), 6)
    expect_equal(igraph::vcount(graph_direct(g, h)), 9)
    expect_equal(igraph::ecount(graph_direct(g, h)), 2)
})

test_that("direct product degrees are products of degrees", {
    set.seed(1)
    g <- igraph::sample_gnp(6, 0.5)
    h <- igraph::sample_gnp(5, 0.5)
    gh <- graph_direct(g, h)
    expect_equal(
        unname(igraph::degree(gh)),
        rep(igraph::degree(g), each = 5) * rep(igraph::degree(h), times = 6)
    )
})

test_that("graph products use vertex names", {
    g <- igraph::make_graph(c("a", "b"), directed = FALSE)
    h <- igraph::make_graph(c("x", "y"), directed = FALSE)
    expect_setequal(igraph::V(graph_cartesian(g, h))$name, c("a-x", "a-y", "b-x", "b-y"))
})
