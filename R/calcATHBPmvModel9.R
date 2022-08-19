#' PMV based on Adaptive Thermal Heat Balance Framework for Model 9
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmvModel9} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBpmvModel9(trm, ta, tr, vel, rh, met, buildingTypeSimple, 
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
#' @return \code{calcATHBpmvModel9} PMV value adapted through the ATHB approach with model 9
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
#' @examples calcATHBpmvModel9(20, 25, 25, .1, 50, 1.1, 'Office', 'Mixed Mode', 
#' 'winter')

calcATHBpmvModel9 <- function(trm, ta, tr, vel, rh, met,buildingTypeSimple, 
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
  PTSVATHBpmv <- 5.078407 + 0.06534761 * LAdpt - 0.8803375 * metAdpt - 0.06490277
  * trm + 2.006503 * mixedMode + 0.7562162 * naturallyVentilated + 0.7919454 *
    office_simple - 0.02015116 * LAdpt * metAdpt - 0.0002187248 * LAdpt * trm + 
    0.05455496 * metAdpt * trm - 0.05243507 * LAdpt * mixedMode - 0.01462814 *
    LAdpt * naturallyVentilated - 1.449924 * metAdpt * mixedMode - 0.003343158 *
    metAdpt * naturallyVentilated + 0.04106376 * trm * mixedMode + 0.008952933 *
    trm * naturallyVentilated - 0.03219659 * LAdpt * office_simple - 0.2007968 *
    metAdpt * office_simple + 0.05069964 * trm * office_simple - 2.94232 * 
    mixedMode * office_simple - 1.786304 * naturallyVentilated * office_simple - 
    0.0005448004 * LAdpt * metAdpt * trm + 0.05216898 * LAdpt * metAdpt * mixedMode
  - 0.0002852032 * LAdpt * metAdpt * naturallyVentilated + 0.0004011372 * LAdpt
  * trm * mixedMode - 0.0009462404 * LAdpt * trm * naturallyVentilated - 
    0.0526229 * metAdpt * trm * mixedMode - 0.02713719 * metAdpt * trm *
    naturallyVentilated + 0.02575407 * LAdpt * metAdpt * office_simple + 
    0.0002068002 * LAdpt * trm * office_simple - 0.06684268 * metAdpt * trm *
    office_simple + 0.01513595 * LAdpt * mixedMode * office_simple + 0.0008952739
  * LAdpt * naturallyVentilated * office_simple + 1.853336 * metAdpt * mixedMode
  * office_simple + 0.3571055 * metAdpt * naturallyVentilated * office_simple - 
    0.05941917 * trm * mixedMode * office_simple - 0.00337051 * trm * 
    naturallyVentilated * office_simple - -0.00000267134 * LAdpt * trm * mixedMode
  + 0.001361413 * LAdpt * metAdpt * trm * naturallyVentilated + 0.0005316192 *
    LAdpt * metAdpt * trm * office_simple - 0.05424961 * LAdpt * metAdpt * 
    mixedMode * office_simple - 0.000232013 * LAdpt * metAdpt * naturallyVentilated
  * office_simple + 0.00171289 * LAdpt * trm * mixedMode * office_simple + 
    0.001751174 * LAdpt * trm * naturallyVentilated * office_simple + 0.08985407
  * metAdpt * trm * mixedMode * office_simple + 0.05547053 * metAdpt * trm * 
    naturallyVentilated * office_simple - 0.00111858 * LAdpt * metAdpt * trm *
    mixedMode * office_simple - 0.002215205 * LAdpt * metAdpt * trm * 
    naturallyVentilated * office_simple
 
PTSVATHBpmv
}
