#validation for UTCI input
validate_UTCI <- function(ta, tr, vel, rh) {
 parameters <- c(ta, tr, vel, rh)
 
 #check if parameter is numeric and not null
 for (parameter in parameters) {
   if(!is.numeric(parameter) || is.null(parameter))
     warning(paste(parameter," is not valid (has to be numeric and not null)"))
 }
 
 #check range for ta & tr
 for (parameter in c(ta, tr)) {
   check_range(parameter, 0, 40)
   }
 
 #check range for vel
 check_range(vel, 0, 10)
 
 #check range for rh
 check_range(rh, 0, 100)
}

#validation for Solar Gain Input
validateSolarGain <- function(solAlt, solAzi, solRadDir, solTrans, fSvv, 
                              fBes, asw=0.7, posture="seated", floorRef=0.6) {
  
  #check if parameters are  not null
  for (parameter in c(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw, 
                      posture, floorRef)) {
    if(is.null(parameter))
      warning(paste(parameter," is not valid (cannot be null)"))
  }
  
  #check if parameters are numeric
  for (parameter in c(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw, floorRef)) {
    if(!is.numeric(parameter))
      warning(paste(parameter," is not valid (has to be numeric)"))
  }
  
  #validate posture input
  if(!posture %in% c("standing", "supine", "seated"))
    stop("Posture has to be either standing, supine or seated")
  
  #check range for Solar altitude
  check_range(solAlt, 0, 90)
  
  #check range for Solar azimuth
  check_range(solAzi, 0, 180)
  
  #check range for Direct-beam solar radiation
  check_range(solRadDir, 200, 1000)
  
  #check ranges for solTrans, fSvv, fBes, asw
  for (parameter in c(solTrans, fSvv, fBes, asw)) {
    check_range(parameter, 0, 1)
  }
}

#check range for parameters
check_range <- function(parameter, lower_bound, upper_bound) {
  if( parameter < lower_bound || parameter > upper_bound)
    stop(paste(parameter, " is out of range. Has to be between ",lower_bound, " and ",upper_bound))
}

#get acceptable utci range
getUtciRange <- function() {
  ta = tr = c(0,20,40)
  vel = c(0,5,10)
  rh = c(0,50,100)
  utciValue = expand.grid(ta = ta, tr = tr, vel = vel, rh = rh, UTCI = utci_approx(ta, tr, vel, rh))
  c(min(utciValue$UTCI), max(utciValue$UTCI))
}

getSolarGainRange <- function() {
  solAlt = c(0, 45, 90) 
  solAzi = c(0, 90, 180) 
  solRadDir = c(200, 1100, 2000) 
  solTrans = fSvv = fBes = asw = c(0,0.5,1)
  posture= c("standing", "supine", "seated")
  solarGainValue = expand.grid(solAlt = solAlt, solAzi = solAzi, 
                               solRadDir = solRadDir, solTrans = solTrans,
                               fSvv = fSvv, fBes = fBes, asw = asw, 
                               solarGainRes = solarGain(solAlt, solAzi, 
                                                        solRadDir, solTrans, 
                                                        fSvv, fBes,asw,posture))
  
  
  #c(min(solarGainValue$erf), max(solarGainValue$erf), 
    #min(solarGainValue$delMrt), max(solarGainValue$delMart))
}
