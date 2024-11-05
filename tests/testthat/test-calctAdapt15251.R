test_that("test calctAdapt15251", {
  reference_tables <- retrieve_data(url_config$test_adaptive_en_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]

    if (isFALSE(data$execute_in_R[i])) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
    }

    expected_output <- outputs$tmp_cmf[[1]]

    if (is.null(expected_output)) {
      next
    }

    result <- calctAdapt15251(inputs$t_running_mean[[1]])
    expect_true(abs(result$tAdapt15251 - expected_output) <= tolerance,
      info = paste(
        "Failed at data row", i, ": Adapt15251 tolerance check. inputs:",
        inputs$t_running_mean[[1]], "outputs: ", outputs$tAdapt15251,
        "real outputs:", result$tAdapt15251
      )
    )
  }
})
