#validation for UTCI
#' @title Validation for Calculating UTCI
#' @description Functions to validate UTCI .
#' @usage call calcUTCI(ta, tr, vel, rh), validate_UTCI will be called automatically
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @details inputs have to be numerical and not null. Also, the values have to be of certain range.
#' @return Gives a warning message if any of the inputs are not numerical, null and out of rage. If the inputs are valid then nothing is done.
#' @author Code implemented in to R by Shaomi Rahman
#' @export
#' @examples
#' validate_UTCI(25, 25, 0.1, 50) #valid input
#' does nothing
#' validate_UTCI(FALSE, 50, 0.1, 50) #not numerical ta input
#' validate_UTCI(50, 50, 0.1, 50) #invalid input for ta, td
#' Warning Message: 50 is out of range. Has to be between 0 and 40"
#'                  50 is out of range. Has to be between 0 and 40
#' validate_UTCI(25, 25, 0.1, 150) #invalid input for rh
#' Warning Message: 150 is out of range. Has to be between 0 and 100"
validate_UTCI <- function(ta, tr, vel, rh) {
 parameters <- c(ta, tr, vel, rh)
 
 #check if parameter is numeric and not null
 for (parameter in parameters) {
   if(!is.numeric(parameter) || is.null(parameter))
     warning(paste(parameter," is not valid (has to be numeric and not null)"))
 }
 
 #check range for ta & tr
 for (parameter in c(ta, tr)) {
   check_range(parameter, 0, 40)
   }
 
 #check range for vel
 check_range(vel, 0, 10)
 
 #check range for rh
 check_range(rh, 0, 100)
}

#check range for parameters
check_range <- function(parameter, lower_bound, upper_bound) {
  if( parameter < lower_bound || parameter > upper_bound)
    warning(paste(parameter, " is out of range. Has to be between ",lower_bound, " and ",upper_bound))
}
