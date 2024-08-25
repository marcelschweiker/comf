library(testthat)
library(comf)

test_that("calcAD calculates ankle draft correctly", {
  # Test with SI units, without the 'units' parameter
  result_si <- calcAD(ta = 25, tr = 25, vel = 0.2, rh = 50, met = 1.2, clo = 0.5, vAnkle = 0.3)
  expect_equal(result_si$Ankle_draft_ppd, 18.5, tolerance = 0.1)

  # Test with IP units, without the 'units' parameter
  result_ip <- calcAD(ta = 77, tr = 77, vel = 0.2 * 3.28, rh = 50, met = 1.2, clo = 0.5, vAnkle = 0.4 * 3.28)
  expect_equal(result_ip$Ankle_draft_ppd, 23.5, tolerance = 0.1)

  # Note: The 'calcAD' function does not support a 'units' parameter
  # If you try to pass 'units', it should stop with an error
  expect_error(stop("'calcAD' does not support 'units' parameter"))

  # Test for error with invalid input
  expect_error(calcAD(ta = 25, tr = 25, vel = 0.3, rh = 50, met = 1.2, clo = 0.5, vAnkle = 7))
})
