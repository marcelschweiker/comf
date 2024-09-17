# Define the test
test_that("test_calcpmvpdd", {
source("../config.R")
source("../utils-test-tool.R")

# Retrieve test data using retrieve_data function
reference_tables <- retrieve_data(url_config$test_pmv_ppd_url)

# Extract data and tolerance from the retrieved reference tables
tolerance <- reference_tables$tolerance
data <- reference_tables$data

# Define tolerance values for PMV and PPD
tolerance_pmv <- tolerance$pmv
tolerance_ppd <- tolerance$ppd

  # Loop through each dataset in the data list
  for (i in seq_len(nrow(data$inputs))) {
    # Extract the current test case inputs and expected outputs
    inputs <- data$inputs[i, ]
    expected <- data$outputs[i, ]
    
    # Run the calcPMVPPD function with the current test case inputs
    result <- calcPMVPPD(inputs$tdb,
                         inputs$tr,
                         inputs$rh,
                         inputs$vr,
                         inputs$met,
                         inputs$clo)
    
    # Compare the results with the expected values using expect_true and abs()
    expect_true(abs(result$pmv - expected$pmv) < tolerance_pmv, 
                info = paste("Test case", i, "failed on PMV values"))
    expect_true(abs(result$ppd - expected$ppd) < tolerance_ppd, 
                info = paste("Test case", i, "failed on PPD values"))
  }
})
