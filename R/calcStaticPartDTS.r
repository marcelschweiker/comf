#' Static Thermal Sensation (from FIALA original model)
#'
#'@aliases calcStaticPartDTS StaticPartDTS PartDTS DTS dts
#' @description 
#' \code{calcStaticPartDTS} calculates Static Thermal Sensation based on updated FANGER DPD model
#'@param ta a numeric value presenting air temperature in [degree C]
#'@param tr a numeric value presenting mean radiant temperature in [degree C]
#'@param rh a numeric value presenting relative humidity [\%]
#'@param vel a numeric value presenting air velocity in [m/s]
#'@param clo a numeric value presenting clothing insulation level in [clo] 
#'@param met a numeric value presenting metabolic rate in [met]
#'
#'@details this function calculates Static Thermal Sensation from FIALA original model
#'@return \code{calcStaticPartDTS} returns the Static Thermal Sensation
#'@author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#'@seealso see also \code{\link{calc2Node}}
#'@examples
#'calcStaticPartDTS(26.92817, 25.50000, 45.00000, 0.14000, 0.60000, 1) # Returns 0.3538098
calcStaticPartDTS <- function(ta, tr, rh, vel, clo, met){
  tskinSet <- 34.6
  tcoreSet <- 37.3
  
  calc2NodeVal <- calc2Node(ta, tr, vel, rh, clo, met)
  
  dtSkin <- calc2NodeVal$tsk - tskinS et
  dtCore <- calc2NodeVal$tcr - tcoreSet
  
  if((dtCore > -0.4) & (dtSkin < 4)){
    g <- 7.94 * exp(-0.902 / (dtCore + 0.4) + 7.612 / (dtSkin - 4))
  }
  else{
    g <- 0
  }
    
  if(dtSkin < 0){
    a <- 0.301
  }
  else{
    a <- 1.078
  }

  sts <- 3 * tanh(a * dtSkin + g)
  
  return(sts)
}
