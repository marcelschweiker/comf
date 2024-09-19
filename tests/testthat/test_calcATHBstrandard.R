library(testthat)
source("../config.R")
source("../utils-test-tool.R")

test_that("Test calcATHBstandard", {
  reference_tables <- retrieve_data(url_config$test_athb_url)

  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data
  
  total_cases <- nrow(data_list)

  result <- numeric(total_cases)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
    }
    
    trm <- as.numeric(inputs$t_running_mean)
    ta <- as.numeric(inputs$tdb)
    tr <- as.numeric(inputs$tr)
    vel <- as.numeric(inputs$vr)
    rh <- as.numeric(inputs$rh)
    met <- as.numeric(inputs$met)

    result[i] <- calcATHBstandard(trm, ta, tr, vel, rh, met)
    
    expected_athb <- as.numeric(outputs[[1]])
    
    expect_true(
      abs(result[i] - expected_athb) < tolerance$athb_pmv,
      info = paste("Test case", i, "failed on ATHB standard values")
    )
  }
})
