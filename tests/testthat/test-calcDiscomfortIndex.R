test_that("test_calcDiscomfortIndex", {
  # URL to get the test data from the configuration
  reference_tables <- retrieve_data(url_config$test_discomfort_index_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  # Walk through each set of test data
  for (i in seq_len(nrow(data))) {
    # Extract input and output data
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    execute_in_R <- data$execute_in_R[i]
    if (!is.na(execute_in_R) && execute_in_R == FALSE) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
    }

    # Call the calcdiscomfort_index function
    result <- calcDiscomfortIndex(
      tdb = inputs$tdb,
      rh = inputs$rh
    )

    # Test the DI value to ensure that it is within the tolerance range
    expect_true(
      all(abs(result$di - outputs$di[[1]]) <= tolerance$di),
      info = paste("Test case", i, "failed on DI values")
    )

    # Test for discomfort conditions
    expect_true(
      all(result$discomfort_condition == outputs$discomfort_condition[[1]]),
      info = paste("Test case", i, "failed on discomfort conditions")
    )
  }
})
