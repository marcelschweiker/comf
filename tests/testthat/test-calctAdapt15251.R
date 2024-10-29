test_that("test calctAdapt15251", {
  reference_tables <- retrieve_data(url_config$test_adaptive_en_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calctAdapt15251(inputs$t_running_mean[[1]])

    expect_true(abs(result$tAdapt15251 - outputs$tmp_cmf[[1]]) < tolerance,
      info = paste(
        "Failed at data row", i, ": Adapt15251 tolerance check. inputs:",
        inputs$trm, "outputs:", outputs$tAdapt15251
      )
    )
  }
})
