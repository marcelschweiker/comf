test_that("test calcCloTout", {
  source("../config.R")
  source("../utils-test-tool.R")

  reference_tables <- retrieve_data(url_config$test_clo_tout_url)
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    
    result <- calcCloTout(tout = inputs$tout, units = inputs$units)

    expected_result <- unlist(outputs$clo_tout)
    
    print("Actual result:")
    print(result)
    print("Expected (unlisted) result:")
    print(expected_result)

    expect_equal(result, expected_result,
      info = paste("Failed at data row", i, ": clo_tout result exact check.")
    )
  }

  expect_error(
    calcCloTout(tout = 27, units = "invalid"),
    "Invalid unit"
  )

  expect_error(
    calcCloTout(tout = "invalid"),
    "tout must be numeric or a list of numeric values"
  )
})