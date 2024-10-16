library(devtools)
library(dplyr)
library(knitr)

testthat::set_max_fails(Inf)
run_and_capture_tests <- function(output_file) {
  sink(output_file)
  on.exit(sink())
  test()
}

output_file <- "test_log.log"
run_and_capture_tests(output_file)