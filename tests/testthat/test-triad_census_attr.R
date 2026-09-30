triad_types <- c(
    "003", "012", "102", "021D", "021U", "021C", "111D", "111U",
    "030T", "030C", "201", "120D", "120U", "120C", "210", "300"
)

aggregate_types <- function(x) {
    as.vector(tapply(x, sub("-.*", "", sub("^T", "", names(x))), sum)[triad_types])
}

test_that("triad census attr validates input", {
    gundir <- igraph::make_full_graph(3, directed = FALSE)
    gnoatt <- igraph::make_full_graph(3, directed = TRUE)
    gchatt <- igraph::make_full_graph(3, directed = TRUE)
    igraph::vertex_attr(gchatt, "test") <- c("a", "b", "c")
    expect_error(triad_census_attr(gundir, "test"), "directed")
    expect_error(triad_census_attr(gnoatt, "test"), "no vertex attribute")
    expect_error(triad_census_attr(gchatt, "test"), "numeric")
    igraph::vertex_attr(gnoatt, "test") <- c(0, 1, 2)
    expect_error(triad_census_attr(gnoatt, "test"), "integers")
})

test_that("triad census attr works on small graphs", {
    gtriad <- igraph::make_full_graph(3, directed = TRUE)
    igraph::vertex_attr(gtriad, "test") <- 1:3
    res <- triad_census_attr(gtriad, "test")
    expect_equal(res[["T300-123"]], 1)
    expect_equal(sum(res), 1)
})

test_that("number of classes matches Burnside count", {
    for (k in 1:4) {
        g <- igraph::make_empty_graph(k)
        igraph::V(g)$a <- seq_len(k)
        res <- triad_census_attr(g, "a")
        expect_length(res, (64 * k^3 + 24 * k^2 + 8 * k) / 6)
        expect_false(anyDuplicated(names(res)) > 0)
    }
})

test_that("cyclic triads keep their orientation", {
    g1 <- igraph::make_graph(c(1, 2, 2, 3, 3, 1), directed = TRUE)
    g2 <- igraph::make_graph(c(1, 3, 3, 2, 2, 1), directed = TRUE)
    igraph::V(g1)$a <- igraph::V(g2)$a <- 1:3
    res1 <- triad_census_attr(g1, "a")
    res2 <- triad_census_attr(g2, "a")
    expect_equal(res1[["T030C-123"]], 1)
    expect_equal(res1[["T030C-321"]], 0)
    expect_equal(res2[["T030C-123"]], 0)
    expect_equal(res2[["T030C-321"]], 1)
})

test_that("triad census attr agrees with igraph and is permutation invariant", {
    set.seed(9)
    for (r in 1:10) {
        n <- 30
        g <- igraph::sample_gnp(n, stats::runif(1, 0.02, 0.4), directed = TRUE)
        igraph::V(g)$a <- sample(1:3, n, replace = TRUE)
        res <- triad_census_attr(g, "a")
        expect_equal(sum(res), choose(n, 3))
        expect_equal(unname(aggregate_types(res)), igraph::triad_census(g))
        h <- igraph::permute(g, sample(n))
        expect_equal(triad_census_attr(h, "a"), res)
    }
})

test_that("triad census attr counts colored triads like brute force", {
    set.seed(4)
    g <- igraph::sample_gnp(12, 0.3, directed = TRUE)
    igraph::V(g)$a <- sample(1:2, 12, replace = TRUE)
    res <- triad_census_attr(g, "a")
    # brute force: census of each induced triad, labelled by its sorted colors
    tr <- utils::combn(12, 3)
    type <- apply(tr, 2, function(x) {
        which(igraph::triad_census(igraph::induced_subgraph(g, x)) == 1)
    })
    colors <- apply(tr, 2, function(x) paste(sort(igraph::V(g)$a[x]), collapse = ""))
    ref <- table(paste(triad_types[type], colors))
    got <- tapply(res, paste(
        sub("-.*", "", sub("^T", "", names(res))),
        vapply(strsplit(sub(".*-", "", names(res)), ""), function(x) paste(sort(x), collapse = ""), "")
    ), sum)
    got <- got[got > 0]
    expect_equal(as.vector(got[names(ref)]), as.vector(ref))
    expect_equal(sum(got), sum(ref))
})

test_that("triad census attr ignores multiple edges, loops and names", {
    set.seed(5)
    g <- igraph::sample_gnp(20, 0.2, directed = TRUE)
    igraph::V(g)$a <- sample(1:2, 20, replace = TRUE)
    res <- triad_census_attr(g, "a")
    gm <- igraph::add_edges(g, c(igraph::as_edgelist(g)[1, ], 1, 1))
    igraph::V(gm)$name <- paste0("v", 1:20)
    expect_equal(triad_census_attr(gm, "a"), res)
})

test_that("triad census attr labels are unambiguous for many categories", {
    set.seed(6)
    g <- igraph::sample_gnp(12, 0.3, directed = TRUE)
    igraph::V(g)$a <- 1:12
    res <- triad_census_attr(g, "a")
    expect_false(anyDuplicated(names(res)) > 0)
    expect_equal(sum(res), choose(12, 3))
    expect_true("T003-1.2.10" %in% names(res))
})
