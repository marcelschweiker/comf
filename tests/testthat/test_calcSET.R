library(jsonlite)
library(testthat)
source("../config.R")
source("../utils-test-tool.R")

test_that("calcSET function returns correct values", {
  reference_tables <- retrieve_data(url_config$test_set_url)

  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data  
  tolerance_set <- tolerance$set
  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    units <- ifelse(is.null(inputs$units) || is.na(inputs$units), "SI", inputs$units)

    if (units == "IP") {
      ta <- (inputs$tdb - 32) * 5/9
      tr <- (inputs$tr - 32) * 5/9
      vel <- inputs$v * 0.3048
    } else {
      ta <- inputs$tdb
      tr <- inputs$tr
      vel <- inputs$v
    }
    rh <- ifelse(is.null(inputs$rh) || is.na(inputs$rh), 50, inputs$rh)
    met <- ifelse(is.null(inputs$met) || is.na(inputs$met), 1.1, inputs$met)
    clo <- ifelse(is.null(inputs$clo) || is.na(inputs$clo), 0.5, inputs$clo)

    result <- calcSET(
      ta = ta,
      tr = tr,
      vel = vel,
      rh = rh,
      met = met,
      clo = clo
    )

    expected_set <- outputs[[1]]
    expect_true(
      abs(result - expected_set) < tolerance_set,
      info = paste("Test case", i, "failed on SET values")
    )
  }
})
