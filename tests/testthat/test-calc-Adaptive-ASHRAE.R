test_that("test calctAdaptASHRAE", {
  reference_tables <- retrieve_data(url_config$test_adaptive_ashrae_url)
  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data

  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    # Print the inputs
    print(paste("Testing row", i, "with inputs:", toString(inputs)))

    # Skip test case if 'execute_in_R' is set to FALSE
    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
      skip(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
    }

    tmmo <- as.numeric(inputs$t_running_mean)

    result <- calctAdaptASHRAE(unlist(tmmo))

    # Handle both list and data frame results
    if (is.data.frame(result)) {
      tmp_cmf <- result$tAdaptASHRAE
    } else {
      tmp_cmf <- unlist(result)
    }

    check_tolerance(
      tmp_cmf, as.numeric(outputs$tmp_cmf), tolerance$tmp_cmf,
      paste(
        "Failed at data row", i, ": AdaptASHRAE tolerance check. Inputs:",
        "t_running_mean =", tmmo,
        "Expected tmp_cmf =", as.numeric(outputs$tmp_cmf),
        "Actual tmp_cmf =", tmp_cmf
      )
    )
  }
})