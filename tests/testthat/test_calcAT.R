test_that("test calcHumx", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_at_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    params <- list()
    for (name in names(inputs)) {
      value <- inputs[[name]]
      if (!is.na(value)) {
        params[[name]] <- value
      }
    }
    result <- do.call(calcAT, params)
    expect_true(abs(result - outputs$at) < tolerance$at,
      info = paste(
        "Failed at data row", i, ": ", "at tolerance check. inputs:",
        inputs, "outputs:", outputs
      )
    )
  }
})
