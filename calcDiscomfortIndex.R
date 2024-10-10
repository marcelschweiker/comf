#' Calculates the Discomfort Index(DI).
#' The Discomfort Index is based on air temperature and humidity.
#' Thermal discomfort prevention strategy.It is compliant with ISO 15265.
#' @param ta a numeric value presenting air temperature in [degree C].
#' @param rh a numeric value presenting relative humidity [\%].
#'
#' @details The Discomfort Index (DI) is an effective temperature based
#' on air temperature and humidity, typically used for warm environments.
#' It is divided into six categories:
#' [1]: DI < 21°C – No discomfort
#' [2]: 21°C ≤ DI < 24°C – Less than 50% feel discomfort
#' [3]: 24°C ≤ DI < 27°C – More than 50% feel discomfort
#' [4]: 27°C ≤ DI < 29°C – Most people feel discomfort
#' [5]: 29°C ≤ DI < 32°C – Severe stress for everyone
#' [6]: DI ≥ 32°C – Medical emergency
#' @return
#'  A list containing:
#'  \itemize{
#'    \item \code{di}: Numeric value(s) of the Discomfort Index (DI)
#'     in degrees Celsius, rounded to 1 decimal place.
#'    \item \code{discomfort_condition}: Character vector or list representing
#'    the discomfort category based on the DI value(s), such as "No discomfort"
#'    or "State of medical emergency".
#'  }
#' @references ISO 15265
#' @example
#' calcdiscomfort_index(tdb=25, rh=50)
#' @author Code implemented into R by Haiyang Liu.
#' @export
calcdiscomfort_index <- function(tdb, rh) {
  # Convert input temperature and humidity to numeric vectors for calculation
  tdb <- as.numeric(tdb)
  rh <- as.numeric(rh)
  
  # Calculate the Discomfort Index (DI)
  di <- tdb - 0.55 * (1 - 0.01 * rh) * (tdb - 14.5)
  
  # Define DI categories
  di_categories <- c(
    "No discomfort",
    "Less than 50% feels discomfort",
    "More than 50% feels discomfort",
    "Most of the population feels discomfort",
    "Everyone feels severe stress",
    "State of medical emergency"
  )
  
  # Function to map DI value to a discomfort condition
  get_discomfort_condition <- function(di_value) {
    if (di_value < 21) {
      return(di_categories[1])
    } else if (di_value < 24) {
      return(di_categories[2])
    } else if (di_value < 27) {
      return(di_categories[3])
    } else if (di_value < 29) {
      return(di_categories[4])
    } else if (di_value < 32) {
      return(di_categories[5])
    } else {
      return(di_categories[6])
    }
  }
  
  # Apply discomfort condition mapping to all DI values
  discomfort_condition <- sapply(di, get_discomfort_condition)
  
  # Return the results as a list
  return(list(
    di = round(di, 1),
    discomfort_condition = discomfort_condition
  ))
}

