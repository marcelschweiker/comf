test_that("Test calcCE function", {
  source("../config.R")
  source("../utils-test-tool.R")

  reference_tables <- retrieve_data(url_config$test_cooling_effect_url)

  tolerance <- reference_tables$tolerance

  data_list <- reference_tables$data

  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
      skip(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
    }

    ta <- as.numeric(inputs$tdb)
    tr <- as.numeric(inputs$tr)
    vel <- as.numeric(inputs$vr)
    rh <- as.numeric(inputs$rh)
    clo <- as.numeric(inputs$clo)
    met <- as.numeric(inputs$met)

    result <- calcCE(ta, tr, vel, rh, clo, met)
    if (is.na(result)) {
      skip(paste("skip due to return result is NA"))
    }
    numbers <- strsplit(result, " ")[[1]]
    result_ce <- as.numeric(numbers[length(numbers)])
    expected_ce <- as.numeric(outputs[[1]])

    expect_true(
      abs(result_ce - expected_ce) < tolerance$ce,
      info = paste(
        "Failed at data row", i, ": cooling_effect tolerance check. Inputs:",
        "tdb =", ta, "tr =", tr, "v =", vel,
        "rh =", rh, "clo =", clo, "met =", met,
        "Expected ce =", expected_ce,
        "Actual ce =", result_ce
      )
    )
  }
})
