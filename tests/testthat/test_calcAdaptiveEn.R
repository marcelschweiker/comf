test_that("test AdaptiveEn", {
  # Load necessary configurations and utilities
  source("../config.R")
  source("../utils-test-tool.R")
  
  # Retrieve test data from the URL defined in the config
  reference_tables <- retrieve_data(url_config$test_adaptive_en_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  # Loop through the rows of test data and check the calcAdaptiveEn function
  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calcAdaptiveEn(
      ta = inputs$tdb,
      tr = inputs$tr,
      t_running_mean = inputs$t_running_mean,
      v = inputs$v
    )
    
  print(result)

    # For each element in the outputs, check that the result matches the expected output within tolerance
    for (output_name in names(outputs)) {
      expect_true(
        all(abs(result[[output_name]] - outputs[[output_name]]) < tolerance[[output_name]]),
        info = paste("Failed at data row", i, "for output", output_name)
      )
    }
  }
  
  # Test with hardcoded cases for additional validation
  ta <- c(25, 25, 23.5)
  tr <- c(25, 25, 23.5)
  t_running_mean <- c(9, 20, 28)
  v <- 0.1
  
  expected <- list(
    tmp_cmf = c(NA, 25.4, 28.0),
    acceptability_cat_i = c(FALSE, TRUE, FALSE),
    acceptability_cat_ii = c(FALSE, TRUE, FALSE),
    acceptability_cat_iii = c(FALSE, TRUE, TRUE),
    tmp_cmf_cat_i_up = c(NA, 27.4, 30.0),
    tmp_cmf_cat_ii_up = c(NA, 28.4, 31.0),
    tmp_cmf_cat_iii_up = c(NA, 29.4, 32.0),
    tmp_cmf_cat_i_low = c(NA, 22.4, 25.0),
    tmp_cmf_cat_ii_low = c(NA, 21.4, 24.0),
    tmp_cmf_cat_iii_low = c(NA, 20.4, 23.0)
  )
  
  result <- calcAdaptiveEn(
    ta = ta,
    tr = tr,
    t_running_mean = t_running_mean,
    v = v
  )
  
  # Compare each element in the result with the expected values
  for (output_name in names(expected)) {
    expect_equal(
      result[[output_name]],
      expected[[output_name]],
      info = paste("Hardcoded case failed for output", output_name)
    )
  }
  
  # Test for error with invalid ta type
  expect_error(
    calcAdaptiveEn(ta = "invalid", tr = 25, t_running_mean = 20, v = 0.1),
    "ta must be numeric"
  )
  
  # Test for error with invalid v type
  expect_error(
    calcAdaptiveEn(ta = 25, tr = 25, t_running_mean = 20, v = "invalid"),
    "v must be numeric"
  )
})
