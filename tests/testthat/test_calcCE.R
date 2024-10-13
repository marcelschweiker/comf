library(testthat)
source("../config.R")
source("../utils-test-tool.R")

test_that("Test calcCE function", {
  reference_tables <- retrieve_data(url_config$test_cooling_effect_url)

  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data
  
  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
        print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
        next
    }

    ta <- as.numeric(inputs$tdb)
    tr <- as.numeric(inputs$tr)
    vel <- as.numeric(inputs$vr)
    rh <- as.numeric(inputs$rh)
    clo <- as.numeric(inputs$clo)
    met <- as.numeric(inputs$met)

    result <- calcCE(ta, tr, vel, rh, clo, met)
    
    if (!is.numeric(result)) {
      stop(paste("Test case", i, "- Result is not numeric:", result))
    }
    
    expected_ce <- as.numeric(outputs[[1]])
    
    # print the results and the expected to debug
    print(paste("Test case", i, "- Result (cooling_effect):", result, "Expected (cooling_effect):", expected_ce))
    
    expect_true(
      abs(result - expected_ce) < tolerance$cooling_effect,
      info = paste("Test case", i, "failed on cooling_effect values")
    )
  }
})
