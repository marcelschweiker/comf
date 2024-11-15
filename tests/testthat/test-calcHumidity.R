test_that("test calcHumx", {
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
    expect_true(abs(result - outputs$humidex) <= 1,
      info = paste(
        "Failed at data row", i, ": humidex tolerance check. inputs:",
        "ta:", inputs$tdb, "rh:", inputs$rh, "expected_outputs:",
        outputs$humidex, "real output:", result
      )
    )
  }
})
