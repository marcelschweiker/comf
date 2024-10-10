test_that("test_calcdiscomfort_index", {  
  source("../config.R") 
  source("../utils-test-tool.R")
  
  # 从配置中获取测试数据的URL
  reference_tables <- retrieve_data(url_config$test_discomfort_index_url)
  tolerance <- reference_tables$tolerance
  data <- reference_tables$data

  # 遍历每一组测试数据
  for (i in seq_along(data)) {
    # 提取输入和输出数据
    inputs <- data[[i]]$inputs
    outputs <- data[[i]]$outputs

    # 调用 calcdiscomfort_index 函数
    result <- calcdiscomfort_index(
      tdb = inputs$tdb,
      rh = inputs$rh
    )

    # 测试 DI 值，确保在容差范围内
    expect_true(
      all(abs(result$di - outputs$di) < tolerance$di),
      info = paste("Test case", i, "failed on DI values")
    )

    # 测试不适条件
    expect_true(
      all(result$discomfort_condition == outputs$discomfort_condition),
      info = paste("Test case", i, "failed on discomfort conditions")
    )
  }
})
