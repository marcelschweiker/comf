library(devtools)
library(dplyr)
library(knitr)

testthat::set_max_fails(Inf)

output_file <- "test_log.log"

run_and_capture_tests <- function(output_file) {
  sink(output_file)
  on.exit(sink())
  test()
}

# Function to parse and print lines from devtools test log
parse_log_lines <- function(output_file) {
  results <- list()
  log_lines <- readLines(output_file)

  for (line in log_lines) {
    # Extract content after the second pipe
    extracted_content <- sub("^(?:[^|]*\\|){2}\\s*", "", line)
    # Check if the line starts with 'v' or 'x' and add to the list appropriately
    if (grepl("^[v]", line)) {
      results[[extracted_content]] <- "PASSED"
    } else if (grepl("^[x]", line)) {
      results[[extracted_content]] <- "FAILED"
    }
  }
  return(results)
}

run_and_capture_tests(output_file)

results <- parse_log_lines(output_file) # Make sure to call the correct function

results_df <- data.frame(
  Test = names(results),
  Result = unlist(results),
  stringsAsFactors = FALSE
)

markdown_table <- kable(results_df, format = "markdown", row.names = FALSE)

print(markdown_table)

readme_contents <- readLines("README.md")

indexes <- which(readme_contents == "========test result========")

if (length(indexes) == 2) {
  start_index <- indexes[1]
  end_index <- indexes[2]

  part1 <- readme_contents[1:start_index]
  part2 <- readme_contents[(end_index):length(readme_contents)]
  new_contents <- c(part1, markdown_table, part2)

} else {
  new_contents <- c(readme_contents, "\n========test result========\n"
                    , markdown_table, "\n========test result========\n")
}
writeLines(new_contents, "README.md")