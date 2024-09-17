test_that("test calcSolarGain", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_solar_gain_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data
  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    sg <- calcSolarGain(
      solAlt = inputs$sol_altitude,
      solAzi = inputs$sharp,
      solRadDir = inputs$sol_radiation_dir,
      solTrans = inputs$sol_transmittance,
      fSvv = inputs$f_svv,
      fBes = inputs$f_bes,
      asw = inputs$asw,
      posture = inputs$posture
    )
    expect_true(abs(sg[1] - outputs$erf) < tolerance$erf,
      info = paste("Failed at data row", i, ": ERF tolerance check.")
    )
    expect_true(abs(sg[2] - outputs$delta_mrt) < tolerance$delta_mrt,
      info = paste("Failed at data row", i, ": delta_mrt tolerance check.")
    )
  }
})
