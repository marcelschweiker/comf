test_that("calcVTG calculates vertical temperature gradient PPD correctly", {
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_vtg_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    if (isFALSE(data$execute_in_R[i])) {
      skip(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
    }
    if (!is.null(inputs$units) && !is.na(inputs$units) && inputs$units == "ip") {
      skip(paste("Skipping test case", i, "due to 'units' being 'ip'"))
    }
    result <-
      calcVTG(
        ta = inputs$tdb,
        tr = inputs$tr,
        vel = inputs$vr,
        rh = inputs$rh,
        clo = inputs$clo,
        met = inputs$met,
        v_tmp_grad = inputs$vertical_tmp_grad
      )

    if (!is.null(result)) {
      expect_true(abs(result$PPD_vg - outputs$ppd_vg) <= tolerance$ppd_vg,
        info = paste(
          "Failed at data row", i, ": PPD_vg tolerance check.",
          "\nInputs:", paste(names(inputs), unlist(inputs), sep = " = ", collapse = ", "),
          "\nExpected PPD_vg:", outputs$ppd_vg,
          "\nActual PPD_vg:", result$PPD_vg,
          "\nDifference:", abs(result$PPD_vg - outputs$ppd_vg),
          "\nTolerance:", tolerance$ppd_vg
        )
      )
    }
  }
  # Test for warning when velocity is too high
  expect_warning(
    calcVTG(
      ta = 25, tr = 25, vel = 0.3, rh = 50,
      met = 1.2, clo = 0.5, v_tmp_grad = 7
    ),
    "Velocity\\(vel\\) should be less than or equal to 0.2"
  )
})
