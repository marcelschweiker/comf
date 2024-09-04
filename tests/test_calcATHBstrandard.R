library(testthat)


test_that("Test calcATHBstandard", {
    # input
    trm <- c(20, 20, 20, 20)
    ta <- c(25, 25, 15, 25)
    tr <- c(25, 35, 25, 25)
    vel <- c(0.1, 0.1, 0.2, 0.1)
    rh <- c(50, 50, 50, 60)
    met <- c(1.1, 1.5, 1.2, 2)

    expected <- c(0.17, 0.912, -0.755, 0.38)

    result <- numeric(length(ta))

    for (i in seq_along(ta)) {
        result[i] <- calcATHBstandard(trm[i], ta[i], tr[i], vel[i], rh[i], met[i])
    }

    expect_equal(round(result, 3), expected)
})
