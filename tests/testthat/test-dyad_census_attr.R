# brute-force reference implementation
dyad_census_ref <- function(g, vattr) {
    a <- igraph::vertex_attr(g, vattr)
    A <- igraph::as_adjacency_matrix(igraph::simplify(g), sparse = FALSE) > 0
    k <- max(a)
    rows <- list()
    for (x in seq_len(k)) {
        for (y in x:k) {
            cnt <- c(ab = 0, ba = 0, sym = 0, null = 0)
            for (i in seq_along(a)) {
                for (j in seq_along(a)) {
                    if (i == j || a[i] != x || a[j] != y || (x == y && i > j)) next
                    type <- if (A[i, j] && A[j, i]) "sym" else if (A[i, j]) "ab" else if (A[j, i]) "ba" else "null"
                    cnt[type] <- cnt[type] + 1
                }
            }
            if (x == y) {
                cnt["ab"] <- cnt["ab"] + cnt["ba"]
                cnt["ba"] <- NA
            }
            rows[[length(rows) + 1]] <- data.frame(
                from_attr = x, to_attr = y, asym_ab = cnt[["ab"]],
                asym_ba = cnt[["ba"]], sym = cnt[["sym"]], null = cnt[["null"]]
            )
        }
    }
    do.call(rbind, rows)
}

expect_census_equal <- function(g, vattr) {
    res <- dyad_census_attr(g, vattr)
    ref <- dyad_census_ref(g, vattr)
    expect_equal(res$from_attr, ref$from_attr)
    expect_equal(res$to_attr, ref$to_attr)
    expect_equal(res$asym_ab, ref$asym_ab)
    expect_equal(res$asym_ba, ref$asym_ba)
    expect_equal(res$sym, ref$sym)
    expect_equal(res$null, ref$null)
}

test_that("dyad census attr validates input", {
    gundir <- igraph::make_full_graph(3, directed = FALSE)
    gnoatt <- igraph::make_full_graph(3, directed = TRUE)
    gchatt <- igraph::make_full_graph(3, directed = TRUE)
    igraph::vertex_attr(gchatt, "test") <- c("a", "b", "c")
    expect_error(dyad_census_attr(gundir, "test"), "directed")
    expect_error(dyad_census_attr(gnoatt, "test"), "no vertex attribute")
    expect_error(dyad_census_attr(gchatt, "test"), "numeric")
    igraph::vertex_attr(gnoatt, "test") <- c(1, 2.5, 3)
    expect_error(dyad_census_attr(gnoatt, "test"), "integers")
    igraph::vertex_attr(gnoatt, "test") <- c(0, 1, 2)
    expect_error(dyad_census_attr(gnoatt, "test"), "integers")
    igraph::vertex_attr(gnoatt, "test") <- c(1, NA, 2)
    expect_error(dyad_census_attr(gnoatt, "test"), "integers")
})

test_that("dyad census attr works on complete graph", {
    gattr <- igraph::make_full_graph(3, directed = TRUE)
    igraph::vertex_attr(gattr, "test") <- c(1, 2, 3)
    res <- dyad_census_attr(gattr, "test")
    expect_equal(res$sym, c(0, 1, 1, 0, 1, 0))
    expect_equal(res$null, rep(0, 6))
})

test_that("dyad census attr counts one-directional asymmetric dyads", {
    g <- igraph::make_graph(c(1, 3), n = 4, directed = TRUE)
    igraph::V(g)$a <- c(1, 1, 2, 2)
    res <- dyad_census_attr(g, "a")
    expect_equal(res$asym_ab, c(0, 1, 0))
    expect_equal(res$null, c(1, 3, 1))
    expect_census_equal(g, "a")
})

test_that("dyad census attr counts within-group asymmetric dyads", {
    g <- igraph::make_graph(c(1, 2), n = 4, directed = TRUE)
    igraph::V(g)$a <- c(1, 1, 2, 2)
    res <- dyad_census_attr(g, "a")
    expect_equal(res$asym_ab, c(1, 0, 0))
    expect_equal(res$asym_ba, c(NA, 0, NA))
    expect_equal(res$null, c(0, 4, 1))
})

test_that("dyad census attr matches brute force on random graphs", {
    set.seed(1)
    for (i in 1:5) {
        g <- igraph::sample_gnp(25, 0.2, directed = TRUE)
        igraph::V(g)$a <- sample(1:3, 25, replace = TRUE)
        expect_census_equal(g, "a")
    }
})

test_that("dyad census attr sums to all dyads", {
    set.seed(2)
    g <- igraph::sample_gnp(30, 0.3, directed = TRUE)
    igraph::V(g)$a <- sample(1:4, 30, replace = TRUE)
    res <- dyad_census_attr(g, "a")
    total <- sum(res$asym_ab, res$asym_ba, res$sym, res$null, na.rm = TRUE)
    expect_equal(total, choose(30, 2))
    dc <- igraph::dyad_census(g)
    expect_equal(sum(res$sym), dc$mut)
    expect_equal(sum(res$asym_ab, res$asym_ba, na.rm = TRUE), dc$asym)
})

test_that("dyad census attr works with named vertices", {
    set.seed(3)
    g <- igraph::sample_gnp(20, 0.2, directed = TRUE)
    igraph::V(g)$a <- sample(1:2, 20, replace = TRUE)
    g_named <- g
    igraph::V(g_named)$name <- paste0("v", 1:20)
    expect_equal(dyad_census_attr(g_named, "a"), dyad_census_attr(g, "a"))
})

test_that("dyad census attr handles empty graphs and unused values", {
    g <- igraph::make_empty_graph(4)
    igraph::V(g)$a <- c(1, 1, 2, 2)
    res <- dyad_census_attr(g, "a")
    expect_equal(res$null, c(1, 4, 1))
    expect_equal(res$asym_ab, c(0, 0, 0))

    g <- igraph::make_graph(c(1, 2, 2, 3), n = 3, directed = TRUE)
    igraph::V(g)$a <- c(1, 3, 3)
    expect_census_equal(g, "a")
})
