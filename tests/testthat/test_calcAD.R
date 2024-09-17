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
      vel = inputs$v,
      rh = inputs$rh,
      met = inputs$met,
      clo = inputs$clo,
      vAnkle = inputs$v_ankle
    )
    expect_true(
      abs(result$Ankle_draft_ppd - outputs$Ankle_draft_ppd)
      < tolerance$Ankle_draft_ppd,
      info = paste("Failed at data row", i, ":Ankle_draft_ppd tolerance check.")
    )
  }
  # Test for error with invalid input
  expect_error(calcAD(ta = 25, tr = 25, vel = 0.3, rh = 50, met = 1.2,
    clo = 0.5, vAnkle = 7
  ))
  # Test that 'units' parameter is not supported
  expect_error(calcAD(ta = 25, tr = 25, vel = 0.3, rh = 50, met = 1.2,
      clo = 0.5, vAnkle = 0.3, units = "SI"
    ), "unused argument"
  )
})