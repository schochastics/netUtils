test_that("sample_pa_homophilic works", {
    g <- sample_pa_homophilic(
        n = 50,
        m = 2,
        minority_fraction = 0.2,
        h_ab = 1
    )
    expect_true(sum(V(g)$minority) == 10)
})
