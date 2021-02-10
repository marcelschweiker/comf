#' Calculating Solar Gain
#'
#' @aliases calcSolarGain calcsolargain SolarGain solargain
#' @description Function to calculate effective radiant field and delta mean radiant temperature.
#'
#' @usage calcSolarGain(solAlt, solAzi, solRadDir, solTrans,
#'  fSvv, fBes, asw=0.7, posture="seated", floorRef=0.6)
#' 
#' @param solAlt a numeric value presenting solar altitude, degrees from horizontal in [degree C]
#' @param solAzi a numeric value presenting solar azimuth, degrees clockwise from North in [degree C]
#' @param solRadDir a numeric value presenting direct-beam solar radiation [W/m2]
#' @param solTrans a numeric value presenting total solar transmittance. Ranges from 0 to 1.
#' @param fSvv a numeric value presenting fraction of sky vault exposed to body. Ranges from 0 to 1.
#' @param fBes a numeric value presenting fraction of the possible body surface exposed to sun. Ranges from 0 to 1.
#' @param asw a numeric value presenting the average short-wave absorptivity of the occupant.
#' @param posture a list of available options 'standing', 'supine' or 'seated'. 
#' @param floorRef a numeric value presenting floor reflectance. Usually assumed to be constant and equal to 0.6.
#'
#' @return An array of two values
#' First values represents \code{erf} - Net energy flux to or from the human body using the Effective Radiant Field [W/m2]
#' Second value represents \code{delMrt} - Delta mean radiant temperature, the increase 
#' in radiant temperature required without solar radiation [Degree C]
#' 
#' @references 
#' Original code in Python by Tartarini & Schiavon (2020) https://doi.org/10.1016/j.softx.2020.100578
#' @examples
#' calcSolarGain(0, 120, 800, 0.5, 0.5, 0.5, asw=0.7, posture="seated") # Returns [42.9, 10.3]
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso see also \code{\link{calcComfInd}}
#' @export
calcSolarGain <- function(solAlt, solAzi, solRadDir, solTrans, 
                          fSvv, fBes, asw=0.7, 
                          posture="seated", floorRef=0.6){
  
  posture = tolower(posture)
  validateSolarGain(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw, 
                    posture)
  solarGainRes = solarGain(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw, 
                           posture, floorRef=0.6)
  
  solarGainRange = c(0.0, 463.3, 0.0, 110.9)
  
  check_range(solarGainRes[1], solarGainRange[1],solarGainRange[2])
  check_range(solarGainRes[2], solarGainRange[3],solarGainRange[4])

  solarGainRes
}

