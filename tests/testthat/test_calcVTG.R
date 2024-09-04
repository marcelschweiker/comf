library(testthat)
library(comf)

test_that("calcVTG calculates vertical temperature gradient PPD correctly", {
  # Tolerance values
  ppd_tolerance <- 0.1

  # Test IP units
  result_ip <- calcVTG(77, 77, 0.328, 50, 1.2, 0.5, 7 / 1.8)
  expect_equal(result_ip$PPD_vg, 13.0)
  expect_equal(result_ip$Acceptability, FALSE)

  # Test SI units
  result_si_1 <- calcVTG(25, 25, 0.1, 50, 1.2, 0.5, 7)
  expect_equal(result_si_1$PPD_vg, 12.6)

  result_si_2 <- calcVTG(25, 25, 0.1, 50, 1.2, 0.5, 4)
  expect_equal(result_si_2$PPD_vg, 1.7)
  expect_equal(result_si_2$Acceptability, TRUE)

  # Test for error when velocity is too high
  expect_error(
    calcVTG(25, 25, 0.3, 50, 1.2, 0.5, 7),
    "Velocity\\(vel\\) should be less than or equal to 0.2"
  )
})
