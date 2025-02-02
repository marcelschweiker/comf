test_that("test WBGT", {
  # Retrieve the test data from the specified URL
  reference_tables <- retrieve_data(url_config$test_wbgt_url)

  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data

  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    # Skip test case if 'execute_in_R' is set to FALSE
    if (!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) {
      skip(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
    }

    # Input parameters
    twb <- as.numeric(inputs$twb)
    tg <- as.numeric(inputs$tg)
    tdb <- if (!is.null(inputs$tdb) && !is.na(inputs$tdb)) as.numeric(inputs$tdb) else NULL
    with_solar_load <- ifelse(is.null(inputs$with_solar_load) || is.na(inputs$with_solar_load), FALSE, as.logical(inputs$with_solar_load))
    round_output <- ifelse(is.null(inputs$round) || is.na(inputs$round), TRUE, as.logical(inputs$round))

    # Call the calcWbgt function to compute the result
    if (is.null(tdb)) {
      result <- calcWbgt(twb = twb, tg = tg, with_solar_load = with_solar_load, round = round_output)
    } else {
      result <- calcWbgt(twb = twb, tg = tg, tdb = tdb, with_solar_load = with_solar_load, round = round_output)
    }

    # Extract expected value
    expected_wbgt <- as.numeric(outputs[[1]])

    # print(
    #   paste(
    #     "Test case", i, "inputs: twb:", twb,
    #     "tg:", tg, "tdb:", tdb, "with_solar_load:", with_solar_load,
    #     "expected output:", expected_wbgt, "real output:", result
    #   )
    # )

    # Validate wbgt
    expect_true(
      abs(result - expected_wbgt) <= tolerance$wbgt,
      info = paste("Test case", i, "failed on wbgt values")
    )
  }
})