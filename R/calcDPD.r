#' Dynamic Percentage of Dissatisfied
#'
#'@aliases calcDPD DPD dpd
#' @description 
#' \code{calcDPD} calculates Dynamic Percentage of Dissatisfied
#'@param ta a numeric value presenting air temperature in [degree C]
#'@param tr a numeric value presenting mean radiant temperature in [degree C]
#'@param rh a numeric value presenting relative humidity [\%]
#'@param vel a numeric value presenting air velocity in [m/s]
#'@param clo a numeric value presenting clothing insulation level in [clo] 
#'@param met a numeric value presenting metabolic rate in [met]
#'
#'@details this function calculates Dynamic Percentage of Dissatisfied from FIALA original model
#'@return \code{calcDPD} returns the Dynamic Percentage of Dissatisfied
#'@author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#'@seealso see also \code{\link{calc2Node}}
#'@examples
#'calcDPD(26.92817, 25.50000,45.00000,0.14000,0.60000,1.00000) # Returns 0
calcDPD <- function(ta, tr, rh, vel, clo, met){
  sts <- calcStaticPartDTS(ta, tr, rh, vel, clo, met)
  dts <- calcNewDTS(ta, tr, rh, vel, clo, met)
  calc2NodeVal <- calc2Node(ta, tr, rh, vel, clo, met)
  
  if(sts >0){
    if(calc2NodeVal$dtsk >0){
      a <- -0.0679554 * tanh(calc2NodeVal$dtsk)
      b <- -0.54240851 * tanh(calc2NodeVal$dtsk)
    }
    else{
      a <- -0.02509763 * tanh(calc2NodeVal$dtsk)
      b <- -0.21511396 * tanh(calc2NodeVal$dtsk)
    }
  }
  else{
    if(calc2NodeVal$dtsk >0){
      a <- 0.02509763 * tanh(calc2NodeVal$dtsk)
      b <- -0.21511396 * tanh(calc2NodeVal$dtsk)
    }
    else{
      a <- 0.0679554 * tanh(calc2NodeVal$dtsk)
      b <- -0.54240851 * tanh(calc2NodeVal$dtsk)
    }
  }
  
  dpd <- (1 - 0.95 * exp(-0.03353 * (dts + b^ 4) - 0.2179 * (dts + b^ 2) + a)) * 100
  return(max(0, dpd))
}
