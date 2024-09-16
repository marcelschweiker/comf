test_that("test calcSolarGain", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_wbgt_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    params <- list()
    for (name in names(inputs)){
      value <- inputs[[name]]
      if (!is.na(value)) {
        params[[name]] <- value
      }
    }
    t_wbg <- do.call(calcWbgt, params)
    expect_equal(t_wbg, outputs$wbgt)
  }
})
