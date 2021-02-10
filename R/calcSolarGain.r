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
#' @param floorRef a numeric value presenting floor refectance. It is assumed to be constant and equal to 0.6.
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
  solarGainRange = c(0.0, 463.3, 0.0, 110.9)
  
  check_range(solarGainRes[1], solarGainRange[1],solarGainRange[2])
  check_range(solarGainRes[2], solarGainRange[3],solarGainRange[4])
  solarGainRes
}

solarGain <- function(solAlt, solAzi, solRadDir, solTrans, 
                      fSvv, fBes, asw=0.7, 
                      posture="seated", floorRef=0.6){
  
  deg_to_rad = 0.0174532925
  hr = 6
  i_diff = 0.2 * solRadDir
  
  fp_table = rbind(
    c(0.25, 0.25, 0.23, 0.19, 0.15, 0.10, 0.06),
    c(0.25, 0.25, 0.23, 0.18, 0.15, 0.10, 0.06),
    c(0.24, 0.24, 0.22, 0.18, 0.14, 0.10, 0.06),
    c(0.22, 0.22, 0.20, 0.17, 0.13, 0.09, 0.06),
    c(0.21, 0.21, 0.18, 0.15, 0.12, 0.08, 0.06),
    c(0.18, 0.18, 0.17, 0.14, 0.11, 0.08, 0.06),
    c(0.17, 0.17, 0.16, 0.13, 0.11, 0.08, 0.06),
    c(0.18, 0.18, 0.16, 0.13, 0.11, 0.08, 0.06),
    c(0.20, 0.20, 0.18, 0.15, 0.12, 0.08, 0.06),
    c(0.22, 0.22, 0.20, 0.16, 0.13, 0.09, 0.06),
    c(0.24, 0.24, 0.21, 0.17, 0.13, 0.09, 0.06),
    c(0.25, 0.25, 0.22, 0.18, 0.14, 0.09, 0.06),
    c(0.25, 0.25, 0.22, 0.18, 0.14, 0.09, 0.06)
  )
  
  if(posture == "seated"){
    fp_table = rbind(
      c(0.20, 0.23, 0.21, 0.21, 0.18, 0.16, 0.12),
      c(0.20, 0.23, 0.20, 0.20, 0.19, 0.16, 0.12),
      c(0.20, 0.23, 0.21, 0.20, 0.18, 0.15, 0.12),
      c(0.19, 0.23, 0.20, 0.20, 0.18, 0.15, 0.12),
      c(0.18, 0.21, 0.19, 0.19, 0.17, 0.14, 0.12),
      c(0.16, 0.20, 0.18, 0.18, 0.16, 0.13, 0.12),
      c(0.15, 0.18, 0.17, 0.17, 0.15, 0.13, 0.12),
      c(0.16, 0.18, 0.16, 0.16, 0.14, 0.13, 0.12),
      c(0.18, 0.18, 0.16, 0.14, 0.14, 0.12, 0.12),
      c(0.19, 0.18, 0.15, 0.13, 0.13, 0.12, 0.12),
      c(0.21, 0.18, 0.14, 0.12, 0.12, 0.12, 0.12),
      c(0.21, 0.17, 0.13, 0.11, 0.11, 0.12, 0.12),
      c(0.21, 0.17, 0.12, 0.11, 0.11, 0.11, 0.12)
    )
  }
  
  if(posture == "supine"){
    alt_temp = solAlt
    solAlt = abs(90 - solAzi)
    solAzi = alt_temp
  }
  
  alt_range = c(0, 15, 30, 45, 60, 75, 90)
  az_range = c(0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180)
  alt_i = find_span(alt_range, solAlt)
  az_i = find_span(az_range, solAzi)
  fp11 = fp_table[az_i,alt_i]
  fp12 = fp_table[az_i,alt_i + 1]
  fp21 = fp_table[az_i + 1,alt_i]
  fp22 = fp_table[az_i + 1,alt_i + 1]
  az1 = az_range[az_i]
  az2 = az_range[az_i + 1]
  alt1 = alt_range[alt_i]
  alt2 = alt_range[alt_i + 1]
  fp = fp11 * (az2 - solAzi) * (alt2 - solAlt)
  fp = fp + (fp21 * (solAzi - az1) * (alt2 - solAlt))
  fp = fp +(fp12 * (az2 - solAzi) * (solAlt - alt1))
  fp = fp +(fp22 * (solAzi - az1) * (solAlt - alt1))
  fp = fp/((az2 - az1) * (alt2 - alt1))
  f_eff = 0.725
  
  if(posture == "seated")
    f_eff = 0.696
  
  sw_abs = asw
  lw_abs = 0.95
  
  e_diff = f_eff * fSvv * 0.5 * solTrans * i_diff
  e_direct = fp * solTrans * fBes * solRadDir
  e_refl = (
    f_eff 
    * fSvv 
    * 0.5 
    * solTrans 
    * (solRadDir * sin(solAlt * deg_to_rad) + i_diff) 
    * floorRef
  )
  e_solar = e_diff + e_direct + e_refl
  erf = e_solar * (sw_abs / lw_abs)
  delMrt = erf / (hr * f_eff)
  return(c(round(erf,1),round(delMrt,1)))
}

find_span <- function(arr, x){
  for(i in seq(1, length(arr)-1)){
    if (arr[i] <= x && arr[i+1] >= x){
      return(i)
    }
  }
  return(-1)
}
