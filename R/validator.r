#' @keywords internal
#' @noRd
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
validateSolarGainRange <- function(solAlt, solAzi, solRadDir, solTrans, fSvv, 
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
validate_pmv_ppd <- function(pmv_ppd_data){
  
  for(pmv_ppd_data_row in pmv_ppd_data){
    for(pmv_ppd_data_row_index in 1:nrow(pmv_ppd_data_row)){
      data_row <- pmv_ppd_data_row[pmv_ppd_data_row_index,]
      inputs <- data_row$inputs
      outputs <- data_row$outputs
      pmv_output <- round(calcPMV(inputs$ta, inputs$tr,inputs$rh, inputs$v, inputs$met, inputs$clo),1)
      ppd_output <- round(calcPPD(inputs$ta, inputs$tr,inputs$rh, inputs$v, inputs$met, inputs$clo),1)
      if((pmv_output!= outputs$pmv) || (ppd_output !=  outputs$ppd)){
        print(paste("Outputs did not match for these inputs ->  ta:",inputs$ta, 
                    ",tr:",inputs$tr ,",rh:" , inputs$rh ,",v:" ,inputs$v , 
                    ",met:" , inputs$met ,",clo:",inputs$clo)) 
        print(paste("Output from our function -> pmv:",pmv_output, ",ppd:", ppd_output))
        print(paste("Original outputs are -> pmv:", outputs$pmv, ",ppd: ", outputs$ppd))
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#validate Solar Gain
validate_solar_gain <- function(solar_gain_data){
  for (solar_gain_data_row in solar_gain_data) {
    for(solar_gain_data_row_index in 1:nrow(solar_gain_data_row)){
      data_row <- solar_gain_data_row[solar_gain_data_row_index,]
      inputs <- data_row$inputs
      outputs <- data_row$outputs
      solar_gain_result <- calcSolarGain(inputs$alt, inputs$sharp, inputs$I_dir, inputs$t_sol, inputs$f_svv, inputs$f_bes, inputs$asa, inputs$posture)
      if((solar_gain_result[1] != outputs$erf) || (solar_gain_result[2] !=  outputs$t_rsw)){
        print(paste("Outputs did not match for these inputs ->  alt:",inputs$alt, 
                ",sharp:",inputs$sharp ,",I_dir:" , inputs$I_dir ,",t_sol:" 
              , inputs$t_sol , ",f_svv:" , inputs$f_svv ,",f_bes:",  
                inputs$f_bes , ",asa:" , inputs$asa , ",posture:", inputs$posture)) 
        print(paste("Output from our function -> erf:",solar_gain_result[1], ",t_rsw:", solar_gain_result[2]))
        print(paste("Original outputs are -> erf:",outputs$erf, ",t_rsw: ", outputs$t_rsw))
                
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#validate SET
validate_set <- function(set_data){
  for(set_data_row in set_data){
    for(set_data_row_index in 1:nrow(set_data_row)){
      data_row <- set_data_row[set_data_row_index,]
      inputs <- data_row$inputs
      output <- data_row$outputs$set
      set_result <- round(calcSET(inputs$ta, inputs$tr, inputs$v, inputs$rh, inputs$clo, inputs$met),1)
      if(set_result != output){
        print(paste("Outputs did not match for these inputs ->  ta:",inputs$ta, 
                    ",tr:",inputs$tr ,",rh:" , inputs$rh ,",v:" ,inputs$v , 
                    ",met:" , inputs$met ,",clo:",inputs$clo)) 
        print(paste("Output from our function -> set:",set_result))
        print(paste("Original output is -> set:", output))
        return(FALSE)
      }
    }
  }
}

# validate all methods
validate_all_functions <- function(){
  validation_data <- as.data.frame(jsonlite::fromJSON("https://raw.githubusercontent.com/FedericoTartarini/validation-data-comfort-models/main/validation_data.json"))
  pmv_ppd_validation_result <- validate_pmv_ppd(validation_data$reference_data.pmv_ppd.data)
  solar_gain_validation_result <- validate_solar_gain(validation_data$reference_data.solar_gain.data)
  set_validation_result <- validate_set(validation_data$reference_data.set.data)
}

validate_all_functions()
