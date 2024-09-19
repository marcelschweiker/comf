test_that("test calcHumx", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_humidex_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calcHumx(
      ta = inputs$tdb,
      rh = inputs$rh
    )
    if (is.na(outputs$humidex)) {
      # Skip this iteration if humidex is missing
      next
    }
    expect_true(abs(result - outputs$humidex) < tolerance$humidex,
      info = paste("Failed at data row", i, ": humidex tolerance check. inputs:"
                   , inputs$tdb, inputs$rh, "outputs:", outputs$humidex)
    )
  }
})
