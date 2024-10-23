test_that("test calctAdaptASHRAE", {
  source("../config.R")
  source("../utils-test-tool.R")
  
  reference_tables <- retrieve_data(url_config$test_adaptive_ashrae_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    result <- calctAdaptASHRAE(inputs$t_running_mean[[1]])

    expect_true(abs(result$tAdaptASHRAE - outputs$tmp_cmf[[1]]) < tolerance,
      info = paste("Failed at data row", i, ": AdaptASHRAE tolerance check. inputs:",
                   inputs$tmmo, "outputs:", outputs$tAdaptASHRAE)
    )
  }
})