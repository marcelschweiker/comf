#' @keywords internal
#' @noRd

library("jsonlite")
validateUTCI <- function(ta, tr, vel, rh) {
  parameters <- c(ta, tr, vel, rh)
  
  #check if parameter is numeric and not null
  for (parameter in parameters) {
    if(!is.numeric(parameter) || is.null(parameter))
      warning(paste(parameter," is not valid (has to be numeric and not null)"))
  }
  
  #check range for ta & tr
  for (parameter in c(ta, tr)) {
    checkRange(parameter, 0, 40)
  }
  
  #check range for vel
  checkRange(vel, 0, 10)
  
  #check range for rh
  checkRange(rh, 0, 100)
}

#validation for Solar Gain Input
validateSolarGain <- function(solAlt, solAzi, solRadDir, solTrans, fSvv, 
                              fBes, asw=0.7, posture) {
  
  #check if parameters are  not null
  for (parameter in c(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw, 
                      posture)) {
    if(is.null(parameter))
      warning(paste(parameter," is not valid (cannot be null)"))
  }
  
  #check if parameters are numeric
  for (parameter in c(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw)) {
    if(!is.numeric(parameter))
      warning(paste(parameter," is not valid (has to be numeric)"))
  }
  
  #validate posture input
  if(!posture %in% c("standing", "supine", "seated"))
    stop("Posture has to be either standing, supine or seated")
  
  #check range for Solar altitude
  checkRange(solAlt, 0, 90)
  
  #check range for Solar azimuth
  checkRange(solAzi, 0, 180)
  
  #check range for Direct-beam solar radiation
  checkRange(solRadDir, 200, 1000)
  
  #check ranges for solTrans, fSvv, fBes, asw
  for (parameter in c(solTrans, fSvv, fBes, asw)) {
    checkRange(parameter, 0, 1)
  }
}

#check range for parameters
checkRange <- function(parameter, lower_bound, upper_bound) {
  if( parameter < lower_bound || parameter > upper_bound)
    stop(paste(parameter, " is out of range. Has to be between ",lower_bound, " and ",upper_bound))
}

# validate calcPMV and calcPPD
validate_pmv_ppd <- function(){
  validate_pmv_ppd_data <- as.data.frame(jsonlite::fromJSON("https://raw.githubusercontent.com/FedericoTartarini/validation-data-comfort-models/main/validation_data.json"))$pmv_ppd[[1]]$data[-(1:2),]
  
  inputs <- validate_pmv_ppd_data$inputs
  outputs <- validate_pmv_ppd_data$outputs
  
  pmv_result <- round(calcPMV(inputs$ta, inputs$tr, inputs$v, inputs$rh, inputs$clo, inputs$met),1)
  ppd_result <- round(calcPPD(inputs$ta, inputs$tr, inputs$v, inputs$rh, inputs$clo, inputs$met),1)
  
  return (pmv_result == outputs$pmv) && (ppd_result == outputs$ppd)
}

# validate all methods
validate_all_functions <- function(){
  pmv_ppd_validation_result <- validate_pmv_ppd()
  
  return(pmv_ppd_validation_result)
}

validate_all_functions()
