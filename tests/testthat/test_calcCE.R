#source("/Users/yiqingzhang/comf/R/calcCE.R")
library(testthat)

test_that("Testing cooling effect", {
  # Define the ranges and expected results
  t_range <- seq(10, 40, 10)
  rh_range <- seq(10, 75, 25)
  v_range <- seq(0.1, 4, 1)
  all_combinations <- expand.grid(t_range, rh_range, v_range)
  expected_results <- c(
    0, 8.19, 10.94, 12.54, 0, 8.05, 10.77, 12.35, 0, 7.91, 10.6, 12.16,
    0, 5.04, 6.62, 7.51, 0, 4.84, 6.37, 7.24, 0, 4.64, 6.12, 6.97, 0, 3.64, 
    4.32, 4.69, 0, 3.55, 4.25, 4.61, 0, 3.4, 4.1, 4.46
  )
  
  # Loop over all combinations and compare with expected results
  for (i in seq_len(nrow(all_combinations))) {
    ta <- all_combinations[i, 1]
    rh <- all_combinations[i, 2]
    vel <- all_combinations[i, 3]
    
    expect_equal(
      round(calcCE(ta, ta, vel, rh, clo = 0.5, met = 1), 2),
      expected_results[i],
      tolerance = 0.1
    )
  }
  
  # Additional specific tests for edge cases
  expect_equal(calcCE(25, 25, 0.05, 50, clo = 0.6, met = 1), 0)
  expect_equal(calcCE(25, 25, 0.5, 50, clo = 0.6, met = 1), 2.17)
  expect_equal(calcCE(27, 25, 0.5, 50, clo = 0.6, met = 1), 1.85)
  expect_equal(calcCE(29, 25, 0.5, 50, clo = 0.6, met = 1), 1.63)
  expect_equal(calcCE(31, 25, 0.5, 50, clo = 0.6, met = 1), 1.42)
  expect_equal(calcCE(25, 27, 0.5, 50, clo = 0.6, met = 1), 2.44)
  expect_equal(calcCE(25, 29, 0.5, 50, clo = 0.6, met = 1), 2.81)
  expect_equal(calcCE(25, 25, 0.2, 50, clo = 0.6, met = 1), 0.67)
  expect_equal(calcCE(25, 25, 0.8, 50, clo = 0.6, met = 1), 2.93)
  expect_equal(calcCE(25, 25, 0.0, 50, clo = 0.6, met = 1), 0)
  expect_equal(calcCE(25, 25, 0.5, 60, clo = 0.6, met = 1), 2.13)
  expect_equal(calcCE(25, 25, 0.5, 80, clo = 0.6, met = 1), 2.06)
  expect_equal(calcCE(25, 25, 0.5, 20, clo = 0.6, met = 1), 2.29)
  expect_equal(calcCE(25, 25, 0.5, 60, clo = 0.6, met = 1.3), 2.84)
  expect_equal(calcCE(25, 25, 0.5, 60, clo = 0.6, met = 1.6), 3.5)
  expect_equal(calcCE(25, 25, 0.5, 60, clo = 0.3, met = 1), 2.41)
  expect_equal(calcCE(25, 25, 0.5, 60, clo = 1, met = 1), 2.05)
  
  # Test when cooling effect cannot be calculated
  expect_equal(calcCE(0, 80, 5, 60, met = 3, clo = 1), 0)
})
