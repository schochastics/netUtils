test_that("str.igraph prints without error and includes key sections", {
    library(igraph)
    set.seed(1)
    g <- sample_gnp(8, 0.4, directed = TRUE)
    g$name <- "test net"
    V(g)$grp <- rep(1:2, 4)
    E(g)$weight <- seq_len(ecount(g))
    out <- paste(capture.output(str(g)), collapse = "\n")
    expect_true(grepl("TEST NET", out))
    expect_true(grepl("directed", out))
    expect_true(grepl("weighted", out))
    expect_true(grepl("Nodes: 8", out))
    expect_true(grepl("Graph Attributes", out))
    expect_true(grepl("Vertex Attributes", out))
    expect_true(grepl("Edge Attributes", out))
})

test_that("str.igraph handles a minimal graph with no attributes or edges", {
    library(igraph)
    g <- make_empty_graph(n = 3, directed = FALSE)
    out <- paste(capture.output(str(g)), collapse = "\n")
    expect_true(grepl("UNNAMED NETWORK", out))
    expect_true(grepl("Edges: 0", out))
    expect_true(grepl("Isolates: 3", out))
})

test_that("str.igraph handles a graph with a single edge", {
    g <- igraph::make_graph(c(1, 2), directed = FALSE)
    out <- paste(capture.output(str(g)), collapse = "\n")
    expect_true(grepl("1--2", out))
})

test_that("str.igraph only marks truncated attributes", {
    g <- igraph::make_graph(c(1, 2), directed = FALSE)
    igraph::V(g)$x <- 1:2
    out <- paste(capture.output(str(g)), collapse = "\n")
    expect_false(grepl("1, 2 ...", out, fixed = TRUE))
    g <- igraph::make_empty_graph(500)
    igraph::V(g)$x <- 1:500
    out <- paste(capture.output(str(g)), collapse = "\n")
    expect_true(grepl("...", out, fixed = TRUE))
})
