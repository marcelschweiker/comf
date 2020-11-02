load("~/Documents/GitHub/comf/data/dfField.RData")
load("~/Documents/GitHub/comf/data/dfASHRAETableG11.RData")
load("~/Documents/GitHub/comf/data/dfISO7730TableD1.RData")
#validation for UTCI
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
 
 #check_range for UTCI
 min = get_utci_range()[1]
 max = get_utci_range()[2]
 check_range(UTCI, min, max)
}

#check range for parameters
check_range <- function(parameter, lower_bound, upper_bound) {
  if( parameter < lower_bound || parameter > upper_bound)
    warning(paste(parameter, " is out of range. Has to be between ",lower_bound, " and ",upper_bound))
}

get_utci_range <- function() {
  samples = Reduce(function(x, y) merge(x, y, all=TRUE), list(dfField, dfASHRAETableG11,dfISO7730TableD1))
  UTCI_Value = data.frame(ta=numeric(0),tr=numeric(0),vel=numeric(0),rh=numeric(0),UTCI_value=numeric(0))
  for(i in 1:nrow(samples)) {
    row <- samples[i,]
    ta <- row$ta
    tr <- row$tr
    vel <- row$vel
    rh <- row$rh
    UTCI_Value[nrow(UTCI_Value) + 1,] = list(ta,tr,vel,rh,calcUTCI(ta,tr,vel,rh))
  }
  c( min(UTCI_Value$UTCI_value), max(UTCI_Value$UTCI_value) )
}
