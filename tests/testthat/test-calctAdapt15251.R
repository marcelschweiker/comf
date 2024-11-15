test_that("test calctAdapt15251", {

  reference_tables <- retrieve_data(url_config$test_adaptive_en_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data
  
  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    input.list <- if (is.null(inputs$t_running_mean[[1]])) list(NA) else as.list(inputs$t_running_mean[[1]])
    output.list <- if (is.null(outputs$tmp_cmf[[1]])) list(NA) else as.list(outputs$tmp_cmf[[1]])
    result <- calctAdapt15251(input.list)

    expect_true(
        all(mapply(function(x, y) {
            (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && abs(x - y) < tolerance[[1]])  
          },
          result, output.list)),
        info = paste(
          "Failed at data row", i, ": Adapt15251 tolerance check. inputs:",
          inputs$trm, "outputs:", outputs$tAdapt15251
        )
      )
  }
})