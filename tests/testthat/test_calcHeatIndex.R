test_that("test calcHeatIndex", {
  source("../config.R")
  source("../utils-test-tool.R")

  reference_tables <- retrieve_data(url_config$test_heat_index_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]

    units <- ifelse(is.null(inputs$units), "SI", inputs$units)

    result <- calcHeatIndex(
      tdb = inputs$tdb[[1]],
      rh = inputs$rh[[1]],
      units = units
    )

    expect_true(abs(result - outputs$hi) < tolerance$hi,
      info = paste("Failed at data row", i, ": Heat Index tolerance check.")
    )
  }
})
