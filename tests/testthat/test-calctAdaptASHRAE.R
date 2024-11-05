test_that("test calctAdaptASHRAE", {
  reference_tables <- retrieve_data(url_config$test_adaptive_ashrae_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    if (isFALSE(data$execute_in_R[i])) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
    }

    result <- calctAdaptASHRAE(inputs$t_running_mean[[1]])

    expect_true(abs(result$tAdaptASHRAE - outputs$tmp_cmf[[1]]) <= tolerance,
      info = paste(
        "Failed at data row", i, ": AdaptASHRAE tolerance check. inputs:",
        inputs$t_running_mean[[1]], "expected outputs:",
        outputs$tmp_cmf[[1]], "real output:", result$tAdaptASHRAE
      )
    )
  }
})
