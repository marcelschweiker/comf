#' Representative clothing insulation Icl as a function of outdoor air temperature at 06:00 a.m [4].
#' 
#' @param tout float, int, or array-like outdoor air temperature at 06:00 a.m., 
#' default in [°C] in [°F] if `units` = 'IP'
#' @param units is {'SI', 'IP'}, select the SI (International System of Units) or 
#' the IP (Imperial Units) system. Defaults to 'SI'.
#' 
#' @details The ASHRAE 55 2020 states that it is acceptable to determine the clothing insulation 
#' Icl using this equation in mechanically conditioned buildings [1]_. Limitations: This equation 
#' may not be accurate for extreme temperature ranges.
#' @return clo : float, int, or array-like. Representative clothing insulation Icl, [clo]
#' @references ASHRAE 55 2020
#' @examples
#' clo_tout(tout=27)
#' clo_tout(tout=[27, 25])
#' @author Code implemented into R by Yiqing Zhang.
#' @export

calcclo_tout <- function(tout, units = "SI") {
  # Check if tout is numeric or a list of numeric values
  if (!is.numeric(tout) && !is.list(tout)) {
    stop("tout must be numeric or a list of numeric values.")
  }
  
  # Convert list to numeric vector if necessary
  if (is.list(tout)) {
    tout <- unlist(tout)
    if (!is.numeric(tout)) {
      stop("Elements of tout list must be numeric values.")
    }
  }

  # Validate the units
  valid_units <- c("SI", "IP")
  if (!(toupper(units) %in% valid_units)) {
    stop(paste("Invalid unit:", units, ". Supported units are", toString(valid_units), "."))
  }

  # Convert units if necessary (from IP to SI)
  if (tolower(units) == "ip") {
    tout <- (tout - 32) * 5/9  # Convert Fahrenheit to Celsius
  }

  # Calculate clo based on tout
  clo <- ifelse(tout < 26, 10^(-0.1635 - 0.0066 * tout), 0.46)
  clo <- ifelse(tout < 5, 0.818 - 0.0364 * tout, clo)
  clo <- ifelse(tout < -5, 1, clo)

  # Round to 2 decimal places
  return(round(clo, 2))
}
