test_that("test calcAD", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_ankle_draft_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data
  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calcAD(
      ta = inputs$tdb,
      tr = inputs$tr,
      vel = inputs$vr,
      rh = inputs$rh,
      met = inputs$met,
      clo = inputs$clo,
      vAnkle = inputs$v_ankle
    )
    expect_true(
      abs(result$ppdAd - outputs$PPD_ad)
      < tolerance$PPD_ad,
      info = paste("Failed at data row", i, ":Ankle_draft_ppd tolerance check.", "|result: ", result$ppdAd, "expect_outputs: ", outputs)
    )
  }
  # Test for error with invalid input
  expect_warning(calcAD(ta = 25, tr = 25, vel = 0.3, rh = 50, met = 1.2,
    clo = 0.5, vAnkle = 7
  ))
})