#' PMV based on Adaptive Thermal Heat Balance Framework for Model 5
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmvModel5} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBpmvModel5(trm, ta, tr, vel, rh, met, buildingType)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param buildingType - building type. Value can be among Multifamily housing, 
#' Office, Others, as a string
#'
#' @return \code{calcATHBpmvModel5} PMV value adapted through the ATHB approach with model 5
#'
#' @references 
#' Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' Schweiker (2022) <doi:10.1111/ina.13018>
#' 
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso see also \code{\link{calcComfInd}}, \code{link{calcATHBpts}}, \code{link{calcATHBset}},
#' @seealso \code{link{calcATHBpmv2015}}
#' @export
#'
#' @examples calcATHBpmvModel5(20, 25, 25, .1, 50, 1.1, 'Office')

calcATHBpmvModel5 <- function(trm, ta, tr, vel, rh, met, buildingType){
  
  buildingTypeValues <- calcbuildingType(buildingType)
  multifamilyHousing <- buildingTypeValues[1]
  office <- buildingTypeValues[2]
  other <- buildingTypeValues[3]
  
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (- 0.1094361 - 0.004727438 * trm + 0.01962739 * metAdpt - 
                   0.1964082 * multifamilyHousing - 0.03726001 * office + 
                   0.08959306 * others - 0.005118155 * trm * metAdpt - 0.00252243
                   * trm * multifamilyHousing + 0.006292135 * trm * office - 
                   0.002479963 * trm * others + 0.2532793 * metAdpt * 
                   multifamilyHousing + 0.03094452 * metAdpt * office + 
                   0.000318072 * trm * metAdpt * multifamilyHousing + 
                   0.0004807997 * trm * metAdpt * office + 0.00111442 * trm * 
                   metAdpt * others)
  
  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)
  
  # predicted thermal sensation vote 
  PTSVATHBpmv <- 5.484304 + 0.0276035 * LAdpt - 0.9602187 * metAdpt - 0.03421333 *
    trm + 0.002971073 * LAdpt * metAdpt + 0.0002264348 * LAdpt * trm
    + 0.01869608 * metAdpt * trm + 0.0002264348 * LAdpt * trm - 0.0002909158 * 
    LAdpt * metAdpt * trm
  PTSVATHBpmv
}
