library(igraph)
test_that("lfr errors work", {
    # tau1 error
    expect_error(sample_lfr(
        n = 128,
        tau1 = 0.5,
        average_degree = 16,
        max_degree = 16,
        mu = 0.1,
        min_community = 32,
        max_community = 32
    ))
    # tau2 error
    expect_error(sample_lfr(
        n = 128,
        tau1 = 2,
        tau2 = 0.5,
        average_degree = 16,
        max_degree = 16,
        mu = 0.1,
        min_community = 32,
        max_community = 32
    ))
    # avg deg error
    expect_error(sample_lfr(
        n = 128,
        tau1 = 2,
        tau2 = 1,
        average_degree = 200,
        max_degree = 16,
        mu = 0.1,
        min_community = 32,
        max_community = 32
    ))
    # avg deg missing error
    expect_error(sample_lfr(
        n = 128,
        tau1 = 2,
        tau2 = 1,
        max_degree = 16,
        mu = 0.1,
        min_community = 32,
        max_community = 32
    ))
    # max deg error
    expect_error(sample_lfr(
        n = 128,
        tau1 = 2,
        tau2 = 1,
        average_degree = 16,
        max_degree = 200,
        mu = 0.1,
        min_community = 32,
        max_community = 32
    ))
    # mu error
    expect_error(sample_lfr(
        n = 128,
        tau1 = 2,
        tau2 = 1,
        average_degree = 16,
        max_degree = 16,
        mu = -1,
        min_community = 32,
        max_community = 32
    ))
    # com error
    expect_error(sample_lfr(
        n = 128,
        tau1 = 2,
        tau2 = 1,
        average_degree = 16,
        max_degree = 16,
        mu = 0.1,
        max_community = 32
    ))
})

test_that("lfr works", {
    g <- sample_lfr(
        n = 128,
        tau1 = 2,
        tau2 = 1,
        average_degree = 16,
        max_degree = 16,
        mu = 0.1,
        min_community = 32,
        max_community = 32
    )
    expect_equal(max(degree(g)), 16)
    expect_equal(max(V(g)$membership), 4)
    expect_equal(sum(V(g)$membership == 1), 32)
})

test_that("lfr is reproducible with set.seed", {
    f <- function() {
        sample_lfr(
            n = 128, average_degree = 16, max_degree = 16, mu = 0.1,
            min_community = 32, max_community = 32
        )
    }
    set.seed(42)
    g1 <- f()
    set.seed(42)
    g2 <- f()
    expect_identical(as_edgelist(g1), as_edgelist(g2))
    expect_identical(V(g1)$membership, V(g2)$membership)
})

test_that("lfr is silent by default", {
    set.seed(1)
    expect_silent(sample_lfr(n = 200, average_degree = 10, max_degree = 20))
})

test_that("lfr returns overlapping memberships", {
    set.seed(2)
    g <- sample_lfr(
        n = 200, average_degree = 10, max_degree = 20, mu = 0.1,
        min_community = 20, max_community = 50, on = 20, om = 2
    )
    expect_equal(vcount(g), 200)
    expect_type(V(g)$membership, "integer")
    expect_length(V(g)$membership, 200)
    mem <- V(g)$memberships
    expect_type(mem, "list")
    expect_equal(sum(lengths(mem) == 2), 20)
    expect_equal(sum(lengths(mem) == 1), 180)
    expect_equal(V(g)$membership, vapply(mem, `[`, integer(1), 1L))
    expect_true(all(unlist(mem) >= 1))

    # non-overlapping graphs also get the memberships attribute
    set.seed(3)
    g <- sample_lfr(n = 200, average_degree = 10, max_degree = 20)
    expect_true(all(lengths(V(g)$memberships) == 1))
    expect_equal(V(g)$membership, unlist(V(g)$memberships))
})

test_that("lfr validates overlap and community parameters", {
    args <- list(n = 200, average_degree = 10, max_degree = 20)
    lfr <- function(...) do.call(sample_lfr, c(args, list(...)))
    expect_error(lfr(on = -1, om = 2), "on must be a non-negative integer")
    expect_error(lfr(on = 1.5, om = 2), "on must be a non-negative integer")
    expect_error(lfr(on = 10, om = -1), "om must be a non-negative integer")
    expect_error(lfr(on = 300, om = 2), "on must not be larger than n")
    expect_error(lfr(on = 10, om = 1), "om must be at least 2")
    expect_error(
        lfr(min_community = 60, max_community = 20),
        "min_community must not be larger than max_community"
    )
    expect_error(
        lfr(min_community = 0, max_community = 20),
        "min_community must be a positive integer"
    )
})

test_that("lfr reports informative errors from the generator", {
    expect_error(
        sample_lfr(n = 100, average_degree = 90, max_degree = 10),
        "average degree is out of range"
    )
    expect_error(
        sample_lfr(n = 100, average_degree = 1.01, max_degree = 10),
        "average degree is out of range"
    )
})
