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
