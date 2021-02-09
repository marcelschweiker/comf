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

getUtciRange <- function() {
  ta = tr = c(0,20,40)
  vel = c(0,5,10)
  rh = c(0,50,100)
  utciValue = expand.grid(ta = ta, tr = tr, vel = vel, rh = rh)
  utci = c()
  for(i in 1:nrow(utciValue)){
    row <- utciValue[i,]
    utci = append(utci, utciApprox(row$ta, row$tr, row$vel, row$rh))
  }
  utciValue$UTCI = utci
  return(c(min(utciValue$UTCI), max(utciValue$UTCI)))
}

getSolarGainRange <- function() {
  solAlt = c(0, 90) 
  solAzi = c(0, 180) 
  solRadDir = c(200, 1000) 
  solTrans = fSvv = fBes = asw = c(0,1)
  posture= c("standing", "supine", "seated")
  solarGainValue = expand.grid(solAlt = solAlt, solAzi = solAzi, 
                               solRadDir = solRadDir, solTrans = solTrans,
                               fSvv = fSvv, fBes = fBes, asw = asw,
                               posture = posture, erf = 0, dMart = 0)
  for(i in 1:nrow(solarGainValue)){
    solarGainRes = solarGain(solarGainValue$solAlt[i], solarGainValue$solAzi[i],
                             solarGainValue$solRadDir[i], 
                             solarGainValue$solTrans[i], solarGainValue$fSvv[i],
                             solarGainValue$fBes[i], solarGainValue$asw[i],
                             solarGainValue$posture[i])
    solarGainValue$erf[i] = solarGainRes[1]
    solarGainValue$dMart[i] = solarGainRes[2]
  }
  c(min(solarGainValue$erf), max(solarGainValue$erf), 
    min(solarGainValue$dMart), max(solarGainValue$dMart))
}
