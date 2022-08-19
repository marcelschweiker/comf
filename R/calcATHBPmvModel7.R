#' PMV based on Adaptive Thermal Heat Balance Framework for Model 7
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmvModel7} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBpmvModel7(trm, ta, tr, vel, rh, met, buildingType, 
#' buildingTypeSimple, coolingStrategyBuilding)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param buildingType - building type. Value can be among Multifamily housing, 
#' Office, Others, as a string
#' @param buildingTypeSimple - simple building type. Value can be among Multifamily housing, 
#' Office as a string
#' @param coolingStrategyBuilding - the process in which the building was 
#' ventilated. Value can be among Mixed Mode','Naturally Ventilated' as a String
#'
#' @return \code{calcATHBpmvModel7} PMV value adapted through the ATHB approach with model 7
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
#' @examples calcATHBpmvModel7(20, 25, 25, .1, 50, 1.1, 'Office', 'Office', 
#' 'Mixed Mode') 

calcATHBpmvModel7 <- function(trm, ta, tr, vel, rh, met, buildingType, 
                              buildingTypeSimple, coolingStrategyBuilding){
  
  coolingStrategyBuildingValues <- calcCoolingStrategyBuilding(coolingStrategyBuilding)
  mixedMode <- coolingStrategyBuildingValues[1]
  naturallyVentilated <- coolingStrategyBuildingValues[2]
  
  buildingTypeSimpleValues <- calcbuildingTypeSimple(buildingTypeSimple)
  multifamilyhousing_simple <- buildingTypeSimpleValues[1]
  office_simple <- buildingTypeSimpleValues[2]
  
  buildingTypeValues <- calcbuildingType(buildingType)
  multifamilyhousing <- buildingTypeValues[1]
  office <- buildingTypeValues[2]
  others <- buildingTypeValues[3]
  
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (0.2513653 - 0.01833956 * trm - 0.1974669 * metAdpt - 
                  0.4454242 * mixedMode - 0.4066424 * naturallyVentilated - 
                  0.289277 * office_simple + 0.006632598 trm * metAdpt + 0.01109526 * 
                  trm * mixedMode + 0.007924701 * trm * naturallyVentilated + 
                  0.3450699 * metAdpt * mixedMode + 0.244827 * metAdpt * 
                  naturallyVentilated + 0.01290494 * trm * office_simple + 0.124179 *
                  metAdpt * office_simple - 0.01587948 * mixedMode * office_simple + 0.2992075
                  * naturallyVentilated * office_simple - 0.01106321 * trm * mixedMode
                  - 0.005775984 * trm * metAdpt * naturallyVentilated - 0.003026444
                   * trm * metAdpt * office_simple + 0.01343438 * trm * mixedMode * 
                  office_simple - 0.002187271 * trm * naturallyVentilated * office_simple + 
                  0.1505071 * metAdpt * mixedMode * office_simple - 0.08740159 * metAdpt
                  * naturallyVentilated * office_simple - 0.01592234 * trm * metAdpt * 
                  mixedMode * office_simple - 0.003597248 * trm * metAdpt * naturallyVentilated
                  * office_simple)
  
  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)$Lraw
  
  # predicted thermal sensation vote 
  PTSVATHBpmv <- 5.885297 + 0.02851858 * LAdpt - 1.040772 * metAdpt - 0.02552265 *
    trm - 3.829359 * multifamilyhousing - 0.4868315 * office - 0.8696179 * others
  - 0.004345955 * LAdpt * metAdpt - 0.0005074471 * LAdpt * trm + 0.003604598 * 
    metAdpt * trm - 0.0141665 * LAdpt * multifamilyhousing - 0.00809153 * LAdpt
  * office + 0.05611318 * LAdpt * others + 2.862155 * metAdpt * multifamilyhousing
  + 0.1198738 * metAdpt * office + 0.3364892 * metAdpt * others + 0.1115303 * 
    trm * multifamilyhousing - 0.01040979 * trm * office + 0.05022837 * trm * 
    others + 0.0008128102 * LAdpt * metAdpt * trm + 0.008433269 * LAdpt * metAdpt
  * multifamilyhousing + 0.01135066 * LAdpt * metAdpt * office - 0.02606559 * 
    LAdpt * metAdpt * others + 0.0003861339 * LAdpt * trm * multifamilyhousing + 
    0.001622545 * LAdpt * trm * office - 0.002609835 * LAdpt * trm * others - 
    0.08376966 * metAdpt * trm * multifamilyhousing + 0.01744289 * metAdpt * trm
  * office - 0.03544268 * metAdpt * trm * others - 0.0007853332 * LAdpt * metAdpt
  * multifamilyhousing - 0.001865034 * LAdpt * metAdpt * trm * office + 
    0.001292969 * LAdpt * metAdpt * trm * others
  
  PTSVATHBpmv
}
