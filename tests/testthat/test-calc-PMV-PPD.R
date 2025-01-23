# Define the test
test_that("test_calcpmvpdd", {
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

    if (!is.na(data$execute_in_R[i]) &&
      data$execute_in_R[i] == FALSE) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
    }

    if (!is.na(inputs$units) && inputs$units == "ip") {
      print(paste("Skipping test case", i, "due to 'units' being 'ip'"))
      next
    }
    ta <- inputs$tdb[[1]]
    tr <- inputs$tr[[1]]
    rh <- inputs$rh[[1]]
    vel <- inputs$vr[[1]]
    met <- inputs$met[[1]]
    clo <- inputs$clo[[1]]
    # Run the calcPMVPPD function with the current test case inputs
    result <- calcPMVPPD(
      ta = ta,
      tr = tr,
      rh = rh,
      vel = vel,
      met = met,
      clo = clo
    )

    # Compare the results with the expected values using expect_true and abs()
    expect_true(abs(result$pmv - expected$pmv[[1]]) <= tolerance_pmv,
      info = paste(
        "Test case", i, "failed on PMV values", "expected pmv: ",
        expected$pmv[[1]], "actual pmv: ", result$pmv
      )
    )
    expect_true(abs(result$ppd - expected$ppd[[1]]) <= tolerance_ppd,
      info = paste(
        "Test case", i, "failed on ppd values", "expected ppd: ",
        expected$ppd[[1]], "actual ppd: ", result$ppd
      )
    )
  }
})
