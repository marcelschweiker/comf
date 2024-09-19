library(testthat)
source("../config.R")
source("../utils-test-tool.R")


test_that("calcUTCI returns correct values", {
  reference_tables <- retrieve_data(url_config$test_utci_url)
  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data
  
  total_cases <- nrow(data_list)
  
  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])
    
    # Check if this test case is skipped (vel is out of range or execute_in_R is false)
    # why we need this?
    # because in R the function don't support the vel > 10 and IP, so it will be useless to test these files.
    if ((!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) ||
        (inputs$v < 0 || inputs$v > 10)) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE or invalid vel range"))
      next
    }

    ta <- as.numeric(inputs$tdb)
    tr <- as.numeric(inputs$tr)
    vel <- as.numeric(inputs$v)
    rh <- as.numeric(inputs$rh)
    
    result <- calcUTCI(
      ta = ta,
      tr = tr,
      vel = vel,
      rh = rh
    )
    
    expected_utci <- outputs[[1]]

    result <- as.numeric(result)
    expected_utci <- as.numeric(expected_utci)

    expect_true(
      abs(result - expected_utci) < tolerance$utci,
      info = paste("Test case", i, "failed on UTCI values")
    )
  }
})
