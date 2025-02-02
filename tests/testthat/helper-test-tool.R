library(httr)
library(jsonlite)

retrieve_data <- function(url) {
  resp <- GET(url)
  if (status_code(resp) != 200) {
    stop("Failed to fetch data: ", status_code(resp))
  }
  data <- fromJSON(content(resp, "text"), flatten = FALSE)
  return(data)
}

check_tolerance <- function(actual, expected, tolerance, message) {
  if (!is.null(expected) && !is.na(expected)) {
    expect_true(
      abs(actual - expected) <= tolerance,
      info = message
    )
  }
}
