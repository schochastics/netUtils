test_that("sample_pa_homophilic works", {
    g <- sample_pa_homophilic(
        n = 50,
        m = 2,
        minority_fraction = 0.2,
        h_ab = 1
    )
    expect_true(sum(igraph::V(g)$minority) == 10)
    expect_equal(igraph::vcount(g), 50)
})

test_that("sample_pa_homophilic respects homophily", {
    set.seed(1)
    g <- sample_pa_homophilic(n = 100, m = 2, minority_fraction = 0.3, h_ab = 0)
    el <- igraph::as_edgelist(g, names = FALSE)
    mino <- igraph::V(g)$minority
    expect_true(all(mino[el[, 1]] == mino[el[, 2]]))
    set.seed(1)
    g <- sample_pa_homophilic(n = 100, m = 2, minority_fraction = 0.3, h_ab = 1)
    el <- igraph::as_edgelist(g, names = FALSE)
    mino <- igraph::V(g)$minority
    expect_true(all(mino[el[, 1]] != mino[el[, 2]]))
})

test_that("sample_pa_homophilic creates m edges per node when possible", {
    set.seed(2)
    g <- sample_pa_homophilic(n = 60, m = 3, minority_fraction = 0.5, h_ab = 0.5, directed = TRUE)
    expect_true(igraph::is_directed(g))
    expect_equal(igraph::ecount(g), (60 - 3) * 3)
    expect_true(igraph::is_simple(g))
})

test_that("sample_pa_homophilic validates input", {
    expect_error(sample_pa_homophilic(5, 5, 0.2, 0.5), "m < n")
    expect_error(sample_pa_homophilic(50, 2, 1.2, 0.5), "minority_fraction")
    expect_error(sample_pa_homophilic(50, 2, 0.2, 2), "h_ab")
})
