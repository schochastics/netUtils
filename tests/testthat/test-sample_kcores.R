test_that("kcore sampling works", {
    library(igraph)
    cores00 <- c(0, 0, 0)
    cores01 <- c(1, 1, 1)
    cores02 <- c(2, 2, 2)
    coresNA <- c(4, 2, 2)
    coresCl <- c(4, 4, 4, 4, 4)
    expect_equal(coreness(sample_coreseq(cores00)), cores00)
    expect_equal(coreness(sample_coreseq(cores01)), cores01)
    expect_equal(coreness(sample_coreseq(cores02)), cores02)
    expect_error(sample_coreseq(coresNA))
    expect_equal(coreness(sample_coreseq(coresCl)), coresCl)
})

test_that("kcore sampling rejects impossible sequences", {
    expect_error(sample_coreseq(c(2, 2)), "valid kcore")
    expect_error(sample_coreseq(1), "valid kcore")
    expect_error(sample_coreseq(c(1.5, 1)), "non-negative integers")
    expect_error(sample_coreseq(c(-1, 1)), "non-negative integers")
})

test_that("kcore sampling reproduces random coreness sequences", {
    set.seed(1)
    for (i in 1:20) {
        g <- igraph::sample_gnp(30, stats::runif(1, 0.05, 0.4))
        k <- igraph::coreness(g)
        h <- sample_coreseq(k)
        expect_equal(sort(igraph::coreness(h)), sort(unname(k)))
        expect_true(igraph::is_simple(h))
    }
})
