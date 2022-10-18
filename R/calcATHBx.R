#' PMV based on Adaptive Thermal Heat Balance Framework for the extended Model
#' 
#' aliases athb ATHB
#' @description \code{calcATHBx} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBx(trm, ta, tr, vel, rh, met, buildingTypeSimple, 
#' coolingStrategyBuilding, seasonSimple)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param buildingTypeSimple - simple building type. Value can be among Multifamily housing, 
#' Office as a string
#' @param coolingStrategyBuilding - the process in which the building was 
#' ventilated. Value can be among Mixed Mode','Naturally Ventilated' as a String
#' @param seasonSimple - season. Value can be among 'Spring','Summer', 'Winter' as a String
#'
#' @return \code{calcATHBx} PMV value adapted through the ATHB approach with extended model
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
#' @examples calcATHBx(20, 25, 25, .1, 50, 1.1, 'Office', 'Mixed Mode', 
#' 'winter')

calcATHBx <- function(trm, ta, tr, vel, rh, met, buildingTypeSimple, 
                              coolingStrategyBuilding, seasonSimple){
  
  coolingStrategyBuildingValues <- calcCoolingStrategyBuilding(coolingStrategyBuilding)
  mixedMode <- coolingStrategyBuildingValues[1]
  naturallyVentilated <- coolingStrategyBuildingValues[2]
  
  buildingTypeSimpleValues <- calcbuildingTypeSimple(buildingTypeSimple)
  multifamilyhousing_simple <- buildingTypeSimpleValues[1]
  office_simple <- buildingTypeSimpleValues[2]
  
  seasonValues <- calcSeason(seasonSimple)
  spring <- seasonValues[1]
  summer <- seasonValues[2]
  winter <- seasonValues[3]
  
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (0.0492898 - 0.009406446 * trm - 0.05899411 * metAdpt - 
                     0.5095279 * mixedMode - 0.3772523 * naturallyVentilated - 
                     0.2533458 * office_simple + 0.423494 * winter + 0.00007407631
                   * trm * metAdpt + 0.01321753 * trm * mixedMode + 0.005853607
                   * trm * naturallyVentilated + 0.3503273 * metAdpt * mixedMode
                   + 0.214488 * metAdpt * naturallyVentilated + 0.01067182 * trm
                   * office_simple + 0.05307894 * metAdpt * office_simple + 
                     0.1881751 * mixedMode * office_simple + 0.4381925 * 
                     naturallyVentilated * office_simple - 0.04706885 * trm * 
                     winter - 0.3280129 * metAdpt * winter + 0.1632258 * mixedMode
                   * winter + 0.5761067 * naturallyVentilated * winter - 0.251526
                   * office_simple * winter - 0.01066394 * trm * metAdpt * 
                     mixedMode - 0.003778129 * trm * metAdpt * naturallyVentilated
                   - 0.00079101 * trm * metAdpt * office_simple +  0.002877893 *
                     trm * mixedMode * office_simple - 0.01057294 * trm * 
                     naturallyVentilated * office_simple - 0.1087657 * metAdpt *
                     mixedMode * office_simple - 0.2429825 * metAdpt * 
                     naturallyVentilated * office_simple + 0.0418354 * trm * 
                     metAdpt * winter + 0.03122896 * trm * mixedMode * winter +
                     0.0006722416 * trm * naturallyVentilated * winter - 
                     0.03861212 * metAdpt * mixedMode * winter - 0.2371913 * 
                     metAdpt * naturallyVentilated * winter + 0.03909724 * trm *
                     office_simple * winter + 0.3107031 * metAdpt * office_simple
                   * winter - 0.1825792 * mixedMode * office_simple * winter -
                     0.6964134 * naturallyVentilated * office_simple * winter - 
                     0.003304702 * trm * metAdpt * mixedMode * office_simple + 
                     0.005696653 * trm * metAdpt * naturallyVentilated * 
                     office_simple - 0.02786317 * trm * metAdpt * mixedMode *
                     winter - 0.01318766 * trm * metAdpt * naturallyVentilated *
                     winter - 0.03615926 * trm * metAdpt * office_simple * winter
                   - 0.03047268 * trm * mixedMode * office_simple * winter + 
                     0.009780934 * trm * naturallyVentilated * office_simple * 
                     winter + 0.1920736 * metAdpt * mixedMode * office_simple * 
                     winter + 0.3590954 * metAdpt * naturallyVentilated * 
                     office_simple * winter + 0.02060399 * trm * metAdpt * 
                     mixedMode * office_simple * winter + 0.004416077 * trm * 
                     metAdpt * naturallyVentilated * office_simple * winter)
  
  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)$Lraw
  
  # predicted thermal sensation vote 
  PTSVATHBpmv <- 1.475068 + 0.04250174 * LAdpt - 1.239833 * metAdpt - 0.08618044 * 
    trm + 0.6653757 * mixedMode + 0.9725228 * naturallyVentilated + 0.3406048 * 
    office_simple + 0.001059505 * LAdpt * trm + 0.07425959 * metAdpt * trm + 
    0.009698339 * LAdpt * mixedMode - 0.01544092 * LAdpt * naturallyVentilated - 
    0.2863978 * metAdpt * mixedMode - 0.1753708 * metAdpt * naturallyVentilated + 
    0.1043185 * trm * mixedMode + 0.00622867 * trm * naturallyVentilated - 
    0.002370277 * LAdpt * office_simple + 0.2078216 * metAdpt * office_simple + 
    0.07463154 * trm * office_simple - 1.565381 * mixedMode * office_simple - 
    2.063706 * naturallyVentilated * office_simple - 0.001703518 * LAdpt * 
    metAdpt * trm - 0.002568801 * LAdpt * trm * mixedMode - 0.001266842 * LAdpt * 
    trm * naturallyVentilated - 0.1081008 * metAdpt * trm * mixedMode - 0.02605285 * 
    metAdpt * trm * naturallyVentilated - 0.001371535 * LAdpt * trm * office_simple - 
    0.08895577 * metAdpt * trm * office_simple - 0.05012175 * LAdpt * mixedMode * 
    office_simple + 0.001086241 * LAdpt * naturallyVentilated * office_simple + 
    0.6610309 * metAdpt * mixedMode * office_simple + 0.5801628 * metAdpt * 
    naturallyVentilated * office_simple - 0.1246268 * trm * mixedMode * 
    office_simple + 0.003203511 * trm * naturallyVentilated * office_simple + 
    0.002534486 * LAdpt * metAdpt * trm * mixedMode + 0.001697271 * LAdpt * 
    metAdpt * trm * naturallyVentilated + 0.001934518 * LAdpt * metAdpt * 
    office_simple + 0.004850281 * LAdpt * trm * mixedMode * office_simple + 
    0.002049072 * LAdpt * trm * naturallyVentilated * office_simple + 0.1469868 * 
    metAdpt * trm * mixedMode * office_simple + 0.05113367 * metAdpt * trm * 
    naturallyVentilated * office_simple - 0.003778733 * LAdpt * metAdpt * trm * 
    mixedMode * office_simple - 0.002529804 * LAdpt * metAdpt * trm * 
    naturallyVentilated * office_simple
  
PTSVATHBpmv
}
