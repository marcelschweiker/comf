test_that("test calcPhs", {
  # call retrieve_data() to get test data
  reference_tables <- retrieve_data(url_config$test_phs_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]

    if (!is.null(data$execute_in_R[i]) &&
      !is.na(data$execute_in_R[i]) &&
      data$execute_in_R[i] == FALSE) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE"))
      next
    }
    params <- list()
    for (name in names(inputs)) {
      value <- inputs[[name]]
      if (is.list(value)) {
        value <- value[[1]]
      }
      if (!is.na(value)) {
        params[[name]] <- value
      }
    }

    result <- do.call(calcPhs, params)

    for (name in names(tolerance)) {
      expect_value <- outputs[[name]]
      if (is.list(expect_value)) {
        expect_value <- expect_value[[1]]
      }
      expect_true(abs(result[[name]] - expect_value) < tolerance[[name]],
        info = paste(
          "Failed at data row", i,
          "error key:", name,
          "expected output:", expect_value,
          "real output:", result[[name]]
        )
      )
    }
  }
})
