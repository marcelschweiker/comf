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
