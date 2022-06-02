#' @keywords internal
#' @noRd

calcCoolingStrategyBuilding <- function(coolingStrategyBuilding){
  coolingStrategyBuildingValue <- tolower(gsub(" ", "", coolingStrategyBuilding))
  return(ifelse(c('mixedmode', 'naturallyventilated')==coolingStrategyBuildingValue, 1, 0))
}

calcbuildingType <- function(buildingType){
  buildingTypeValue <- tolower(gsub(" ", "", buildingType))
  return(ifelse(c('multifamilyhousing', 'office', 'others')==buildingTypeValue, 1, 0))
}

calcbuildingTypeSimple <- function(buildingTypeSimple){
  buildingTypeSimpleValue <- tolower(gsub(" ", "", buildingTypeSimple))
  return(ifelse(c('multifamilyhousing', 'office')==buildingTypeSimpleValue, 1, 0))
}

