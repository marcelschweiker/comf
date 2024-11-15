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
    tmmo <- if (is.null(inputs$t_running_mean[[1]])) list(NA) else as.list(inputs$t_running_mean[[1]])
    units <- if ("units" %in% names(inputs)) inputs$units[[1]] else "si"
    output.list <- if (is.null(outputs$tmp_cmf[[1]])) list(NA) else as.list(outputs$tmp_cmf[[1]])
    result <- calctAdaptASHRAE(tmmo, units = units)

    expect_true(
      all(mapply(
        function(x, y) {
          (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && abs(x - y) < tolerance[[1]])
        },
        result, output.list
      )),
      info = paste(
        "Failed at data row", i, ": AdaptASHRAE tolerance check. inputs:",
        inputs$tmmo, "outputs:", outputs$tAdaptASHRAE
      )
    )
  }
})
