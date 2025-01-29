test_that("calcSET function returns correct values", {
  reference_tables <- retrieve_data(url_config$test_set_url)

  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data
  tolerance_set <- tolerance$set

  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    # Skip test case if 'execute_in_R' is set to FALSE
    if (!is.na(data_list$execute_in_R[i]) &&
      data_list$execute_in_R[i] == FALSE) {
      skip(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
    }

    ta <- inputs$tdb
    tr <- inputs$tr
    vel <- inputs$v
    rh <- as.numeric(inputs$rh)
    clo <- as.numeric(inputs$clo)
    met <- as.numeric(inputs$met)


    result <- calcSET(
      ta = ta,
      tr = tr,
      vel = vel,
      rh = rh,
      met = met,
      clo = clo
    )

    expected_set <- outputs[[1]]

    expect_true(
      abs(result - expected_set) < tolerance_set,
      info = paste(
        "Failed at data row", i, ": set_tmp tolerance check. Inputs:",
        "tdb =", ta, "tr =", tr, "v =", vel,
        "rh =", rh, "clo =", clo, "met =", met,
        "Expected set =", expected_set,
        "Actual set =", result
      )
    )
  }
})
