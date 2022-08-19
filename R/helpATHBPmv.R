#' @keywords internal
#' @noRd

calcCoolingStrategyBuilding <- function(coolingStrategyBuilding){
  coolingStrategyBuildingValue <- tolower(gsub(" ", "", coolingStrategyBuilding))
  return(ifelse(c('mixedmode', 'naturallyventilated')==coolingStrategyBuildingValue, 1, 0))
}

calcbuildingType <- function(buildingType){
  buildingTypeValue <- tolower(gsub(" ", "", buildingType))
  multifamilyhousing <- if(buildingType=='multifamilyhousing') 1 else 0
  office <- if(buildingType=='office') 1 else 0
  others <- if(buildingType=='others') 1 else 0
  return(c(multifamilyhousing, office, others))
}

calcbuildingTypeSimple <- function(buildingTypeSimple){
  buildingTypeSimpleValue <- tolower(gsub(" ", "", buildingTypeSimple))
  return(ifelse(c('multifamilyhousing', 'office')==buildingTypeSimpleValue, 1, 0))
}

calcSeason <- function(season){
  seasonValue <- tolower(gsub(" ", "", season))
  spring <- if(seasonValue=='spring') 1 else 0
  summer <- if(seasonValue=='summer') 1 else 0
  winter <- if(seasonValue=='winter') 1 else 0
  return(c(spring, summer, winter))
}
