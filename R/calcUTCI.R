#' Calculating Universal Thermal Comfort Index (UTCI)
#'
#' @aliases calcUTCI calcutci utci UTCI
#'
#' @usage calcUTCI(ta, tr, vel, rh)
#'
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#'
#' @return the \code{utciValue} value rounded to one decimal
#' @export
#' @description Functions to calculate UTCI.
#' @details air temperature and mean radiant temperature should be in Degree C unit. Air velocity has to be in m/s unit and relative humidity has to be put in percentage value.
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @references 
#' UTCI project page on http://www.utci.org/ 
#' Original code in Python by Tartarini & Schiavon (2020) https://doi.org/10.1016/j.softx.2020.100578
#' @seealso see also \code{\link{calcComfInd}}
#' @examples 
#' calcUTCI(25, 25, 1.0, 50) # Returns 24.6
calcUTCI <- function(ta, tr, vel, rh) {
  
  #validate the inputs and prints an error message for invalid inputs
  validateUTCI(ta, tr, vel, rh)
  
  #calculate utci value from function
  utciValue = utciApprox(ta, tr, vel, rh)
  
  #check if utci value is within acceptable range
  if(!exists("utciRange")){
    assign("utciRange", getUtciRange(), envir = .GlobalEnv)
  }
  checkRange(utciValue, utciRange[1], utciRange[2])
  
  #return the value
  utciValue
}

