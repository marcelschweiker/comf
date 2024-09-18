library(jsonlite)
library(testthat)
source("../config.R")
source("../utils-test-tool.R")

reference_tables <- retrieve_data(url_config$test_set_url)

tolerance <- reference_tables$tolerance
data_list <- reference_tables$data  
tolerance_set <- tolerance$set
print(tolerance_set)

test_that("calcSET function returns correct values", {
  total_cases <- nrow(data_list)

  for (i in seq_len(total_cases)) {
    inputs <- as.list(data_list$inputs[i, ])
    outputs <- as.list(data_list$outputs[i, ])

    # 检查是否需要单位转换
    units <- ifelse(is.null(inputs$units) || is.na(inputs$units), "SI", inputs$units)

    # 如果有单位转换需求，进行相应的转换
    if (units == "IP") {
      ta <- (inputs$tdb - 32) * 5/9
      tr <- (inputs$tr - 32) * 5/9
      vel <- inputs$v * 0.3048
    } else {
      ta <- inputs$tdb
      tr <- inputs$tr
      vel <- inputs$v
    }

    # 处理缺失值，设置默认参数
    rh <- ifelse(is.null(inputs$rh) || is.na(inputs$rh), 50, inputs$rh)
    met <- ifelse(is.null(inputs$met) || is.na(inputs$met), 1.1, inputs$met)
    clo <- ifelse(is.null(inputs$clo) || is.na(inputs$clo), 0.5, inputs$clo)

    # 调用 calcSET 函数，传入当前测试用例的输入
    result <- calcSET(
      ta = ta,
      tr = tr,
      vel = vel,
      rh = rh,
      met = met,
      clo = clo
    )

    # 获取期望的 SET 值
    expected_set <- outputs[[1]]

    # 打印输出值和期望值进行对比
    print(paste("Test case", i, "- Result:", result, "Expected:", expected_set))

    # 确保计算结果是数值并且比较误差
    expect_true(
      abs(result - expected_set) < tolerance_set,
      info = paste("Test case", i, "failed on SET values")
    )
  }
})
