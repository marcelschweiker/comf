#' PMV based on Adaptive Thermal Heat Balance Framework for Model 6
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmvModel6} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBpmvModel6(trm, ta, tr, vel, rh, met, buildingTypeSimple)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param buildingTypeSimple - simple building type. Value can be among Multifamily housing, 
#' Office as a string
#'
#' @return \code{calcATHBpmvModel6} PMV value adapted through the ATHB approach with model 6
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
#' @examples calcATHBpmvModel6(20, 25, 25, .1, 50, 1.1, 'Office')

calcATHBpmvModel6 <- function(trm, ta, tr, vel, rh, met, coolingStrategyBuilding){
  
  coolingStrategyBuildingValues <- calcCoolingStrategyBuilding(coolingStrategyBuilding)
  mixedMode <- coolingStrategyBuildingValues[1]
  naturallyVentilated <- coolingStrategyBuildingValues[2]
  
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (- 0.1123245 - 0.01087445 * trm + 0.05153978 * metAdpt - 
                     0.03437166 * office - -0.00002396242 * trm * metAdpt + 
                     0.01243914 * trm * office - 0.0009678703 * metAdpt * office
                   - 0.004613393 * trm * metAdpt * office)
  
  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)
  
  # predicted thermal sensation vote 
  PTSVATHBpmv <- 5.44284 + 0.05631419 * LAdpt - 0.8013034 * metAdpt - 0.01383006 *
    trm + 0.9470648 * mixedMode - 0.198033 * naturallyVentilated - 0.01159431 * 
    LAdpt * metAdpt - 0.0009856706 * LAdpt * trm - 0.007108725 * metAdpt * trm
  - 0.07966168 * LAdpt * mixedMode - 0.03464086 * LAdpt * naturallyVentilated - 
    1.046041 * metAdpt * mixedMode - 0.125155 * metAdpt * naturallyVentilated - 
    0.04408968 * trm * mixedMode - 0.02770695 * trm * naturallyVentilated + 
    0.0004066135 * LAdpt * metAdpt * trm + 0.05287239 * LAdpt * metAdpt * 
    mixedMode + 0.01187241 *  LAdpt * metAdpt * naturallyVentilated + 
    0.003071338 * LAdpt * trm * mixedMode + 0.001353364 * LAdpt * trm * 
    naturallyVentilated + 0.04660158 * metAdpt * trm * mixedMode + 0.04521563 *
    metAdpt * trm * naturallyVentilated - 0.002065213 * LAdpt * metAdpt * trm * 
    mixedMode - 0.0008613593 * LAdpt * metAdpt * trm * naturallyVentilated
    
  PTSVATHBpmv
}
