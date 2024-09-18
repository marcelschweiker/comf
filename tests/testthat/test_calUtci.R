# 引入必要的包和工具
library(testthat)
source("../config.R")
source("../utils-test-tool.R")


test_that("calcUTCI returns correct values", {

  # 获取动态的测试数据
  reference_tables <- retrieve_data(url_config$test_utci_url)
  
  # 获取容忍度和数据列表
  tolerance <- reference_tables$tolerance
  data_list <- reference_tables$data
  
  # 获取总的测试用例数
  total_cases <- nrow(data_list)
  
  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])
    
    # 检查是否跳过此测试用例（vel 超出范围或 execute_in_R 为 false）
    if ((!is.null(data_list$execute_in_R[i]) && !is.na(data_list$execute_in_R[i]) && data_list$execute_in_R[i] == FALSE) ||
        (inputs$v < 0 || inputs$v > 10)) {
      print(paste("Skipping test case", i, "due to 'execute_in_R' being FALSE or invalid vel range"))
      next
    }

    ta <- as.numeric(inputs$tdb)
    tr <- as.numeric(inputs$tr)
    vel <- as.numeric(inputs$v)
    rh <- as.numeric(inputs$rh)
    
    result <- calcUTCI(
      ta = ta,
      tr = tr,
      vel = vel,
      rh = rh
    )
    
    expected_utci <- outputs[[1]]
    
    # 如果 expected_utci 是 NULL 或 NA，跳过此比较
    if (is.null(expected_utci) || is.na(expected_utci)) {
      print(paste("Skipping test case", i, "because expected UTCI is NULL or NA"))
      next
    }

    # 确保结果和期望值都是数值
    result <- as.numeric(result)
    expected_utci <- as.numeric(expected_utci)

    # 输出调试信息：查看类型和值
    print(paste("Test case", i, "- Result:", result, "Expected:", expected_utci))
    print(paste("Types - Result:", class(result), "Expected:", class(expected_utci)))

    expect_true(
      abs(result - expected_utci) < tolerance$utci,
      info = paste("Test case", i, "failed on UTCI values")
    )
  }
})
