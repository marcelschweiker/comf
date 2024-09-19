library(testthat)
source("../config.R")
source("../utils-test-tool.R")

test_that("calcePMV returns correct values", {
  reference_tables <- retrieve_data(url_config$test_e_pmv_url)
  
  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data
  
  total_cases <- nrow(data_list)
  
  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])
    
    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
      print(paste("Skipping test case", i, "because 'execute_in_R' is FALSE"))
      next
    }

    ta <- as.numeric(inputs$tdb)
    tr <- as.numeric(inputs$tr)
    vel <- as.numeric(inputs$vr)
    rh <- as.numeric(inputs$rh)
    met <- as.numeric(inputs$met)
    clo <- as.numeric(inputs$clo)
    epCoeff <- as.numeric(inputs$e_coefficient)
    
    result <- calcePMV(
      ta = ta,
      tr = tr,
      vel = vel,
      rh = rh,
      clo = clo,
      met = met,
      epCoeff = epCoeff
    )
    
    expected_pmv <- outputs[[1]]

    expect_true(
      all(abs(result$epmv - expected_pmv) < tolerance$pmv),
      info = paste("Test case", i, "failed on epmv values")
    )
  }
})
