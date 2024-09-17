test_that("test calcHumx", {
  source("../config.R")
  source("../utils-test-tool.R")
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_humidex_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calcHumx(
      ta = inputs$tdb,
      rh = inputs$rh
    )
    expect_true(abs(result - outputs$humidex) < tolerance$humidex,
      info = paste("Failed at data row", i, ": humidex tolerance check.")
    )
    # Test rounding
    rounded_result <- round(result)
    expect_true(is.numeric(rounded_result) && rounded_result %% 1 == 0,
      info = paste("Failed at data row", i, ": rounding check.")
    )
  }
  # Test input validation
  expect_error(calcHumx("25", 50), "non-numeric argument")
  expect_error(calcHumx(25, "50"), "non-numeric argument")
  # Test boundary conditions
  expect_error(calcHumx(25, 110), "rh must be between 0 and 100")
  expect_error(calcHumx(25, -10), "rh must be between 0 and 100")

  # Test discomfort categorization
  categorize_discomfort <- function(humidex) {
    if (humidex < 30) return("Little or no discomfort")
    if (humidex < 35) return("Noticeable discomfort")
    if (humidex < 40) return("Evident discomfort")
    if (humidex < 45) return("Intense discomfort; avoid exertion")
    if (humidex < 54) return("Dangerous discomfort")
    return("Heat stroke probable")
  }

  discomfort_test_cases <- list(
    list(ta = 10, rh = 50, expected = "Little or no discomfort"),
    list(ta = 28, rh = 50, expected = "Noticeable discomfort"),
    list(ta = 30, rh = 50, expected = "Evident discomfort"),
    list(ta = 35, rh = 50, expected = "Intense discomfort; avoid exertion"),
    list(ta = 40, rh = 50, expected = "Dangerous discomfort")
  )

  for (case in discomfort_test_cases) {
    humidex <- calcHumx(case$tdb, case$rh)
    discomfort_level <- categorize_discomfort(humidex)
    expect_equal(discomfort_level, case$expected,
      info = paste("Failed discomfort categorization for ta =",
                   case$tdb, "and rh =", case$rh)
    )
  }
})