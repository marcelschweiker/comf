#' Calculating Solar Gain
#'
#' @usage calcSolarGain(solAlt, solAzi, solRadDir, solTrans,
#'  fSvv, fBes, asw=0.7, posture="seated", floorRef=0.6)
#' 
#' @param solAlt a numeric value presenting Solar altitude, degrees from horizontal in [degree C]
#' @param solAzi a numeric value presenting Solar azimuth, degrees clockwise from North in [degree C]
#' @param solRadDir a numeric value presenting Direct-beam solar radiation [W/m2]
#' @param solTrans a numeric value presenting Total solar transmittance. Ranges from 0 to 1
#' @param fSvv a numeric value presenting fraction of sky vault exposed to body, ranges from 0 to 1
#' @param fBes a numeric value presenting fraction of the possible body surface exposed to sun, ranges from 0 to 1
#' @param asw a numeric value presenting the average short-wave absorptivity of the occupant. Set to 0.7 by default
#' @param posture Default 'seated' list of available options 'standing', 'supine' or 'seated'
#' @param floorRef a numeric value presenting floor reflectance. It is assumed to be constant and equal to 0.6.
#'
#' @return An array of 2 values
#' First values represents \code{erf} - Solar gain to the human body using the Effective Radiant Field [W/m2]
#' Second value represents \code{delMrt} - Delta mean radiant temperature [Degree C]
#' 
#' @description Functions to calculate Solar Gain.
#' @details Inputs are - 
#' Solar altitude, Solar azimuth should be in Degree C unit. 
#' Direct-beam solar radiation has to be in W/m2.
#' Solar transmittance, Fraction of sky vault, fraction of the possible body surface, fraction of the possible body surface, average short-wave absorptivity of the occupant are decimal values. Has to be from 0 to 1. 
#' Posture is a string input. Has be of one followings - 'standing', 'supine' or 'seated'. 
#' The outputs are - 
#' Solar gain to the human body using the Effective Radiant Field. The unit is W/m2
#' Delta mean radiant temperature in Degree C
#' @export
#' @aliases calcSolarGain calcsolargain SolarGain solargain
#' @examples
#' calcSolarGain(0, 120, 800, 0.5, 0.5, 0.5, asw=0.7, posture="seated") # Returns [42.9, 10.3]
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso see also \code{\link{calcComfInd}}
calcSolarGain <- function(solAlt, solAzi, solRadDir, solTrans, 
                          fSvv, fBes, asw=0.7, 
                          posture="seated", floorRef=0.6){
  
  posture = tolower(posture)
  validateSolarGain(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw, 
                    posture)
  solarGainRes = solarGain(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw, 
                           posture, floorRef=0.6)
  if(!exists("solarGainRange")){
    assign("solarGainRange", getSolarGainRange(), envir = .GlobalEnv)
  }
  checkRange(solarGainRes[1], solarGainRange[1],solarGainRange[2])
  checkRange(solarGainRes[2], solarGainRange[3],solarGainRange[4])
  solarGainRes
}

