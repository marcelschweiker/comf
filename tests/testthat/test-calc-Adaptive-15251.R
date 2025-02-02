test_that("test calctAdapt15251", {
  reference_tables <- retrieve_data(url_config$test_adaptive_en_url)
  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data

  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    # # Print the inputs
    # print(paste("Testing row", i, "with inputs:", toString(inputs)))

    # Skip test case if 'execute_in_R' is set to FALSE
    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
      skip(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
    }

    trm <- as.numeric(inputs$t_running_mean)

    result <- calctAdapt15251(unlist(trm))

    # Handle both list and data frame results
    if (is.data.frame(result)) {
      tmp_cmf <- result$tAdapt15251
    } else {
      tmp_cmf <- unlist(result)
    }

    # Unlist outputs$tmp_cmf if it is a list
    expected_tmp_cmf <- if (is.list(outputs$tmp_cmf)) unlist(outputs$tmp_cmf) else as.numeric(outputs$tmp_cmf)

    check_tolerance(
      tmp_cmf, expected_tmp_cmf, tolerance$tmp_cmf,
      paste(
        "Failed at data row", i, ": Adapt15251 tolerance check. Inputs:",
        "t_running_mean =", trm,
        "Expected tmp_cmf =", expected_tmp_cmf,
        "Actual tmp_cmf =", tmp_cmf
      )
    )
  }
})