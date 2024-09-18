test_that("test calcHeatIndex", {
  # Load necessary configurations and utilities
  source("../config.R")
  source("../utils-test-tool.R")
  
  # Retrieve test data from the URL defined in the config
  reference_tables <- retrieve_data(url_config$test_heat_index_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  # Loop through the rows of test data and check the calcHeatIndex function
  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]

    result <- calcHeatIndex(
      temp = inputs$temp,
      rh = inputs$rh,
      units = inputs$units
    )

    # Assert that the calculated result is within the acceptable tolerance
    expect_true(abs(result - outputs$result) < tolerance$result,
      info = paste("Failed at data row", i, ": Heat Index tolerance check.")
    )
  }

  # Test with hardcoded cases for additional validation
  hardcoded_cases <- data.frame(
    temp = c(25, 30, 77, 86),
    rh = c(50, 80, 50, 80),
    units = c("SI", "SI", "IP", "IP"),
    expected = c(25.9, 37.7, 78.6, 99.8)
  )

  for (i in seq_len(nrow(hardcoded_cases))) {
    result <- calcHeatIndex(
      temp = hardcoded_cases$temp[i],
      rh = hardcoded_cases$rh[i],
      units = hardcoded_cases$units[i]
    )
    
    expect_equal(result, hardcoded_cases$expected[i], 
      info = paste("Hardcoded case failed at row", i))
  }
})