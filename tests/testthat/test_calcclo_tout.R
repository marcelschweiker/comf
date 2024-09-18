test_that("test calcclo_tout", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_clo_tout_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    
    result <- calcclo_tout(tout = inputs$tout, units = inputs$units)

    # Check results for scalar and array inputs
    expect_true(abs(result - outputs$result) < tolerance$result,
      info = paste("Failed at data row", i, ": clo_tout result tolerance check.")
    )
  }

  # Test for error with invalid units
  expect_error(
    calcclo_tout(tout = 27, units = "invalid"),
    "Invalid unit"
  )
  
  # Test for error with invalid tout type
  expect_error(
    calcclo_tout(tout = "invalid"),
    "tout must be numeric or a list of numeric values"
  )
})