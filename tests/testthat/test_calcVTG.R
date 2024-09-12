test_that("calcVTG calculates vertical temperature gradient PPD correctly", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_vtg_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data
  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calcVTG(
      ta = inputs$tdb,
      tr = inputs$tr,
      vel = inputs$v,
      rh = inputs$rh,
      clo = inputs$clo,
      met = inputs$met,
      v_tmp_grad = inputs$delta_t
    )
    expect_true(abs(result$PPD_vg - outputs$PPD_vg) < tolerance$PPD_vg,
      info = paste("Failed at data row", i, ": PPD_vg tolerance check.")
    )
    expect_equal(result$Acceptability, outputs$Acceptability,
      info = paste("Failed at data row", i, ": Acceptability check.")
    )
  }
  # Test for error when velocity is too high
  expect_error(calcVTG(ta = 25, tr = 25, vel = 0.3, rh = 50,
                       met = 1.2, clo = 0.5, v_tmp_grad = 7),
               "Velocity\\(vel\\) should be less than or equal to 0.2")
})
