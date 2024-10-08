test_that("test calcHumx", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_pet_steady_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calcPetSteady(
      tdb = inputs$tdb,
      tr = inputs$tr,
      rh = inputs$rh,
      v = inputs$v,
      met = inputs$met,
      clo = inputs$clo
    )
    expect_true(abs(result - outputs) < tolerance$PET,
      info = paste(
        "Failed at data row", i, ": humidex tolerance check. inputs:",
        inputs$tdb, inputs$tr, inputs$rh, inputs$v, "outputs:", outputs
      )
    )
  }
})
