library(testthat)
source("../config.R")
source("../utils-test-tool.R")


test_that("Test wc function", {
  # Retrieve the test data from the specified URL
  reference_tables <- retrieve_data(url_config$test_wind_chill_index_url)
  
  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data
  
  total_cases <- nrow(data_list)
  
  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])
    
    # Input parameters
    tdb <- as.numeric(inputs$tdb)
    v <- as.numeric(inputs$v)
    # Map 'round' from inputs to 'round_output' in the function
    round_output <- ifelse(is.null(inputs$round) || is.na(inputs$round), TRUE, as.logical(inputs$round))
    
    # Call the wc function to compute the result
    result <- calcWci(tdb = tdb, v = v, round_output = round_output)
    
    # Extract expected value
    expected_wci <- as.numeric(outputs[[1]])
    
    # Validate wci
    expect_true(
      abs(result$wci - expected_wci) < tolerance$wci,
      info = paste("Test case", i, "failed on wci values")
    )
  }
})
