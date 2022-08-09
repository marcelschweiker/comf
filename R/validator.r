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

validateStaticPartDTS_df <- data.frame(matrix(ncol = 8, nrow = 0))
validateStaticPartDTS_colname <- c("ta", "tr", "rh","vel","clo", "met", "StaticPartDTS_comf", "StaticPartDTS")
colnames(validateStaticPartDTS_df) <- validateStaticPartDTS_colname
validateStaticPartDTS <- function(){
  load('dfDPD.RData')
  for (i in seq(1, nrow(output))) {
    data_row <- output[i,]
    ta <- data_row$TA
    tr <- data_row$TR
    rh <- data_row$RH
    vel <- data_row$VEL
    clo <- data_row$CLO
    met <- data_row$MET
    output_comf <- calcStaticPartDTS(ta, tr, rh, vel, clo, met)
    validateStaticPartDTS_df[nrow(validateStaticPartDTS_df)+1,] <- c(ta, tr, rh, vel, clo, met, output_comf, data_row$STS)
  }
}

validateNewDTS_df <- data.frame(matrix(ncol = 8, nrow = 0))
validateNewDTS_colname <- c("ta", "tr", "rh","vel","clo", "met", "NewDTS_comf", "NewDTS")
colnames(validateNewDTS_df) <- validateNewDTS_colname
validateNewDTS <- function(){
  for (i in seq(1, nrow(output))) {
    data_row <- output[i,]
    ta <- data_row$TA
    tr <- data_row$TR
    rh <- data_row$RH
    vel <- data_row$VEL
    clo <- data_row$CLO
    met <- data_row$MET
    output_comf <- calcNewDTS(ta, tr, rh, vel, clo, met)
    validateNewDTS_df[nrow(validateNewDTS_df)+1,] <- c(ta, tr, rh, vel, clo, met, output_comf, data_row$DTS)
  }
}


validateDPD_df <- data.frame(matrix(ncol = 8, nrow = 0))
validateDPD_colname <- c("ta", "tr", "rh","vel","clo", "met", "NewDTS_comf", "NewDTS")
colnames(validateDPD_df) <- validateDPD_colname
validateDPD <- function(){
  for (i in seq(1, nrow(output))) {
    data_row <- output[i,]
    ta <- data_row$TA
    tr <- data_row$TR
    rh <- data_row$RH
    vel <- data_row$VEL
    clo <- data_row$CLO
    met <- data_row$MET
    output_comf <- calcDPD(ta, tr, rh, vel, clo, met)
    validateDPD_df[nrow(validateDPD_df)+1,] <- c(ta, tr, rh, vel, clo, met, output_comf, data_row$DPD)
  }
}
