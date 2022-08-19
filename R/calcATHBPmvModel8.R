#' PMV based on Adaptive Thermal Heat Balance Framework for Model 8
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmvModel8} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBpmvModel8(trm, ta, tr, vel, rh, met, 
#' buildingTypeSimple, coolingStrategyBuilding, season)
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
#' @param season - season. Value can be among 'Spring','Summer', 'Winter' as a String
#'
#' @return \code{calcATHBpmvModel8} PMV value adapted through the ATHB approach with model 8
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
#' @examples calcATHBpmvModel8(20, 25, 25, .1, 50, 1.1, 'Office', 'Mixed Mode', 
#' 'winter')

calcATHBpmvModel8 <- function(trm, ta, tr, vel, rh, met,buildingTypeSimple, 
                              coolingStrategyBuilding, season){
  
  coolingStrategyBuildingValues <- calcCoolingStrategyBuilding(coolingStrategyBuilding)
  mixedMode <- coolingStrategyBuildingValues[1]
  naturallyVentilated <- coolingStrategyBuildingValues[2]
  
  buildingTypeSimpleValues <- calcbuildingTypeSimple(buildingTypeSimple)
  multifamilyhousing_simple <- buildingTypeSimpleValues[1]
  office_simple <- buildingTypeSimpleValues[2]
  
  seasonValues <- calcSeason(season)
  spring <- seasonValues[1]
  summer <- seasonValues[2]
  winter <- seasonValues[3]
  
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (0.02823583 - 0.009476057 * trm - 0.07274179 * metAdpt + 
                     0.2223055 * mixedMode - 0.1170707 * naturallyVentilated + 
                     0.5667378 * office_simple - 0.1043362 * spring + 0.1216924
                   * summer + 0.444548 * winter + 0.003544965 * trm * metAdpt - 
                     0.01587988 * trm * mixedMode - 0.005939769 * trm * 
                     naturallyVentilated - 0.08501531 * metAdpt * mixedMode + 
                     0.1156855 * metAdpt * naturallyVentilated - 0.02773013 * 
                     trm * office_simple - 0.4743845 * metAdpt * office_simple - 
                     1.528042 * mixedMode * office_simple - 0.7303743 * 
                     naturallyVentilated * office_simple + 0.009806725 * trm * 
                     spring - 0.003651348 * trm * summer - 0.04699924 * trm * 
                     winter + 0.1242938 * metAdpt * spring - 0.07327016 * metAdpt
                   * summer - 0.3142653 * metAdpt * winter - 1.016107 * mixedMode
                   * spring - 0.02491184 * naturallyVentilated * spring - 0.8836092
                   * mixedMode * summer - 0.6377072 * naturallyVentilated * 
                     summer - 0.5686077 * mixedMode * winter + 0.3159251 * 
                     naturallyVentilated * winter - 0.9113595 * office_simple * 
                     spring - 0.9175595 * office_simple * summer - 1.07161 * 
                     office_simple * winter + 0.004198499 * trm * metAdpt * 
                     mixedMode - 0.001171735 * trm * metAdpt * naturallyVentilated
                   + 0.02262995 * trm * metAdpt * office_simple + 0.07787021 * 
                     trm * mixedMode * office_simple + 0.05802044 * 
                     naturallyVentilated * office_simple + 1.074345 * metAdpt *
                     mixedMode * office_simple + 0.708444 * metAdpt * 
                     naturallyVentilated * office_simple - 0.01260521 * trm * 
                     metAdpt * spring - 0.0001143546 * trm * metAdpt * summer + 
                     0.03836451 * trm * metAdpt * winter + 0.03316732 * trm * 
                     mixedMode * spring - 0.01016309 * trm * naturallyVentilated
                   * spring + 0.03593691 * trm * mixedMode * summer + 0.03185189 
                   * trm * naturallyVentilated * summer + 0.06032637 * trm * 
                     mixedMode * winter + 0.01246562 * trm * naturallyVentilated
                   * winter + 0.5758421 * metAdpt * mixedMode * spring - 0.1121972
                   * metAdpt * naturallyVentilated * spring + 0.5544738 * metAdpt
                   * mixedMode * summer + 0.3583488 * metAdpt * naturallyVentilated
                   * summer + 0.3967305 * metAdpt * mixedMode * winter - 
                     0.1383887 * metAdpt * naturallyVentilated * winter + 
                     0.04224511 * trm * office_simple * spring + 0.04231909 * trm
                   * office_simple * summer + 0.07749919 * trm * office_simple *
                     winter + 0.6638468 * metAdpt * office_simple * spring + 
                     0.6090887 * metAdpt * office_simple * summer + 0.8381666 *
                     metAdpt * office_simple * winter + 2.649013 * mixedMode * 
                     office_simple * spring + 1.551072 * naturallyVentilated *
                     office_simple * spring + 1.75976 * mixedMode * office_simple
                   * summer + 1.555126 * naturallyVentilated * office_simple * 
                     summer + 1.533638 * mixedMode * office_simple * winter + 
                     0.4721534 * naturallyVentilated * office_simple * winter - 
                     0.05258 * trm * metAdpt * mixedMode * office_simple - 
                     0.04978335 * trm * metAdpt * naturallyVentilated * 
                     office_simple - 0.01298482 * trm * metAdpt * mixedMode * 
                     spring + 0.01738859 * trm * metAdpt * naturallyVentilated *
                     spring - 0.02125965 * trm * metAdpt * mixedMode * summer - 
                     0.01768591 * trm * metAdpt * naturallyVentilated * summer - 
                     0.0427256 * trm * metAdpt * mixedMode * winter - 0.01579405
                   * trm * metAdpt * naturallyVentilated * winter - 0.02956372 *
                     trm * metAdpt * office_simple * spring - 0.02677217 * trm *
                     metAdpt * summer - 0.05958022 * trm * metAdpt * winter - 
                     0.1072282 * trm * mixedMode * office_simple * spring - 
                     0.06808511 * trm * naturallyVentilated * office_simple * 
                     spring - 0.0818576 * trm * mixedMode * office_simple * 
                     summer - 0.08941442 * trm * naturallyVentilated * 
                     office_simple * summer - 0.105465 * trm * mixedMode * 
                     office_simple * winter - 0.05881244 * trm * naturallyVentilated
                   * office_simple * winter - 1.734778 * metAdpt * mixedMode * 
                     office_simple * spring - 1.114612 * metAdpt * naturallyVentilated
                   * office_simple * spring - 1.265676 * metAdpt * mixedMode * 
                     office_simple * summer - 1.218388 * metAdpt * naturallyVentilated
                   * office_simple * summer - 0.9910369 * metAdpt * mixedMode *
                     office_simple * winter - 0.5923311 * metAdpt * naturallyVentilated
                   * office_simple * winter + 0.06921779 * trm * metAdpt * 
                     mixedMode * office_simple * spring + 0.05182579 * trm *
                     metAdpt * naturallyVentilated * office_simple * spring + 
                     0.05768497 * trm * metAdpt * mixedMode * office_simple * 
                     summer + 0.07108423 * trm * metAdpt * naturallyVentilated * 
                     office_simple + 0.06987929 * trm * metAdpt * mixedMode * 
                     office_simple * winter + 0.05989608 * trm * metAdpt * 
                     naturallyVentilated * office_simple * winter)
  
  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)$Lraw
  
  # predicted thermal sensation vote 
  PTSVATHBpmv <- 5.963443 + 0.03181016 * LAdpt - 1.243264 * metAdpt - 0.04171345 *
    trm - 0.5649782 * office_simple - 0.00327195 * LAdpt * metAdpt - 0.000565921
  * LAdpt * trm + 0.02057738 * metAdpt * trm - 0.01138311 * LAdpt * office_simple
  + 0.3223656 * metAdpt * office_simple + 0.005781013 * trm * office_simple + 
    0.0003740362 * LAdpt * metAdpt * trm + 0.01027666 * LAdpt * metAdpt * 
    office_simple + 0.001681019 * LAdpt * trm * office_simple + 0.0004701064 *
    metAdpt * trm * office_simple - 0.00142626 * LAdpt * metAdpt * office_simple)
  
  PTSVATHBpmv
}
