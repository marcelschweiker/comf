#' @noRd

calcCoolingStrategyBuilding <- function(coolingStrategyBuilding){
  coolingStrategyBuildingValue <- tolower(gsub(" ", "", coolingStrategyBuilding))
  return(ifelse(c('mixedmode', 'naturallyventilated')==coolingStrategyBuildingValue, 1, 0))
}

calcBuildingType <- function(buildingType){
  buildingTypeValue <- tolower(gsub(" ", "", buildingType))
  multiFamilyHousing <- if(buildingType=='multiFamilyHousing') 1 else 0
  office <- if(buildingType=='office') 1 else 0
  others <- if(buildingType=='others') 1 else 0
  return(c(multiFamilyHousing, office, others))
}

calcBuildingTypeSimple <- function(buildingTypeSimple){
  buildingTypeSimpleValue <- tolower(gsub(" ", "", buildingTypeSimple))
  return(ifelse(c('multiFamilyHousing', 'office')==buildingTypeSimpleValue, 1, 0))
}

calcSeason <- function(season){
  seasonValue <- tolower(gsub(" ", "", season))
  spring <- if(seasonValue=='spring') 1 else 0
  summer <- if(seasonValue=='summer') 1 else 0
  winter <- if(seasonValue=='winter') 1 else 0
  return(c(spring, summer, winter))
}

p_sat <- function(tdb) {
  # Constants for the equation
  c_to_k <- 273.15
  ta_k <- tdb + c_to_k

  c1 <- -5674.5359
  c2 <- 6.3925247
  c3 <- -0.9677843 * 1e-2
  c4 <- 0.62215701 * 1e-6
  c5 <- 0.20747825 * 1e-8
  c6 <- -0.9484024 * 1e-12
  c7 <- 4.1635019
  c8 <- -5800.2206
  c9 <- 1.3914993
  c10 <- -0.048640239
  c11 <- 0.41764768 * 1e-4
  c12 <- -0.14452093 * 1e-7
  c13 <- 6.5459673

  # Calculate pascals depending on temperature
  pascals <- ifelse(
    ta_k < c_to_k,
    exp(
      c1 / ta_k +
        c2 +
        ta_k * (c3 + ta_k * (c4 + ta_k * (c5 + c6 * ta_k))) +
        c7 * log(ta_k)
    ),
    exp(
      c8 / ta_k +
        c9 +
        ta_k * (c10 + ta_k * (c11 + ta_k * c12)) +
        c13 * log(ta_k)
    )
  )

  # Round the results
  return(round(pascals, 1))
}