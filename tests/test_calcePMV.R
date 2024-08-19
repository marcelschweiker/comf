test_that("calcePMV returns correct values", {
    result <- calcePMV(c(24, 30), 30, vel = 0.22, rh = 50, clo = 0.5, met = 1.4, epCoeff = 0.6)
    expect_equal(result$epmv, c(0.29, 0.91))
})
