# Define the test
test_that("Test calcaPMV function outputs", {

  actual_results <- calcaPMV(c(24, 30), 30, vel=0.22, rh=50, met=1.4, clo=0.5, apCoeff=0.293)

  # Define the expected results
  expected_results <- c(0.48, 1.09)

  # Test if the actual results match the expected results
  expect_equal(actual_results, expected_results)
})