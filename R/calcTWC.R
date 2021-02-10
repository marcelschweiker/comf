#' @title Calculates TWC.
#' @description Function to calculate WINDCHILL TEMPERATURE, TWC in Degrees.
#' @aliases  calctwc
#' @aliases  windchill
#' @aliases  TWC
#' @aliases  twc
#' @usage calcTWC(v,ta)
#' @usage windchill(v,ta)
#' @param ta a numeric value presenting ambient air temperature in [degree C]
#' @param v a numeric value presenting meteorological wind speed (at 10 m) in [km/h]
#' @details  The function returns the temperature that considers the cooling effect on a localized skin segment.
#' @returns returns (twc) Wind chill temperature in [Degree C]
#' @examples calcTWC(6.8,-25)
#' @examples windchill(6.8,-25)
#' @references ISO 11079, 2007-12-15, ERGONOMICS OF THE THERMAL ENVIRONMENT - DETERMINATION AND INTERPRETATION OF COLD STRESS WHEN USING REQUIRED CLOTHING INSULATION (IREQ) AND LOCAL COOLING EFFECTS
#' @note The authors disclaim all obligations and liabilities for damages arising from the use or attempted use of the information, including, but not limited to, direct, indirect, special and consequential damages, and attorneys' and experts' fees and court costs. Any use of the information will be at the risk of the user.
#' @author Developed by Ingvar Holmer and Hakan O. Nilsson, 1990 in java and transferred to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @export
calcTWC <- function(v,ta){

  v = v*1.0
  twc = 13.12 + 0.6215 * ta - 11.37 * (v^0.16) + 0.3965 * ta * (v^0.16)
  twc = round(twc)
  return(twc)
}

windchill<- calcTWC


