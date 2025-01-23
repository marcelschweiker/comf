test_that("test calcAD", {
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_ankle_draft_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    if (isFALSE(data$execute_in_R[i])) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
    }
    if (!is.null(inputs$units) && !is.na(inputs$units) && inputs$units == "ip") {
      print(paste("Skipping test case", i, "due to 'units' being 'ip'"))
      next
    }

    result <- suppressWarnings(calcAD(
      ta = inputs$tdb,
      tr = inputs$tr,
      vel = inputs$vr,
      rh = inputs$rh,
      met = inputs$met,
      clo = inputs$clo,
      vAnkle = inputs$v_ankle
    ))

    cat("\nTesting row", i, "\n")
    cat("Inputs:", paste(names(inputs), inputs, sep = "=", collapse = ", "), "\n")
    cat("Expected output:", outputs$PPD_ad, "\n")
    cat("Actual output:", result$ppdAd, "\n")
    cat("Difference:", abs(result$ppdAd - outputs$PPD_ad), "\n")
    cat("Tolerance:", tolerance$PPD_ad, "\n")

    check_tolerance(
          result$ppdAd, as.numeric(outputs$ppd_ad), tolerance$ppd_ad,
          paste(
            "Failed at data row", i, "Inputs:",
            "tdb =", ta, ", tr =", tr, ", v =", vel,
            ", rh =", rh, ", clo =", clo, ", met =", met,
            "Expected m_rsw =", as.numeric(outputs$ppd_ad),
            "Actual m_rsw =", result$ppd_ad
          )
        )
  }
})
