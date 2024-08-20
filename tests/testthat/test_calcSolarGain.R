test_that("test calcSolarGain", {
  library(httr)
  library(jsonlite)

  # Set the URL
  url <- "https://raw.githubusercontent.com/FedericoTartarini/validation-data-comfort-models/main/validation_data.json"

  # Perform the HTTP GET request
  resp <- GET(url)
  if (status_code(resp) != 200) {
    stop("Failed to fetch data: ", status_code(resp))
  }

  # Parse the JSON data
  reference_tables <- fromJSON(content(resp, "text"))
  # use to output json str for test
  # print(toJSON(reference_tables$reference_data$solar_gain, pretty = TRUE))
  data <- reference_tables$reference_data$solar_gain$data[[1]]
  for (i in seq_len(nrow(data))) {
    inputs <- data[i, "inputs"]
    outputs <- data[i, "outputs"]
    sg <- calcSolarGain(
    solAlt = inputs$alt,
    solAzi = inputs$sharp,
    solRadDir = inputs$I_dir,
    solTrans = inputs$t_sol,
    fSvv = inputs$f_svv,
    fBes = inputs$f_bes,
    asw = inputs$asa,
    posture = inputs$posture
    )
    expect_equal(sg[1], outputs$erf)
    expect_equal(sg[2], outputs$t_rsw)
  }
})
