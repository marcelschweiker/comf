test_that("test_calcDiscomfortIndex", {  
  source("../config.R") 
  source("../utils-test-tool.R")
  
 # URL to get the test data from the configuration
  reference_tables <- retrieve_data(url_config$test_discomfort_index_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

# Walk through each set of test data
  for (i in seq_along(data)) {
# Extract input and output data
    inputs <- data[[i]]$inputs
    outputs <- data[[i]]$outputs

# Call the calcdiscomfort_index function
    result <- calcdiscomfort_index(
      tdb = inputs$tdb,
      rh = inputs$rh
    )

# Test the DI value to ensure that it is within the tolerance range
    expect_true(
      all(abs(result$di - outputs$di) < tolerance$di),
      info = paste("Test case", i, "failed on DI values")
    )

# Test for discomfort conditions
    expect_true(
      all(result$discomfort_condition == outputs$discomfort_condition),
      info = paste("Test case", i, "failed on discomfort conditions")
    )
  }
})
