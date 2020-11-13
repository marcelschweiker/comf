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

#check range for parameters
check_range <- function(parameter, lower_bound, upper_bound) {
  if( parameter < lower_bound || parameter > upper_bound)
    stop(paste(parameter, " is out of range. Has to be between ",lower_bound, " and ",upper_bound))
}

#get acceptable utci range
get_utci_range <- function() {
  ta = tr = seq(from = 0.0, to = 40.0)
  vel = seq(from = 0.0, to = 10.0)
  rh = seq(from = 0.0, to = 100.0)
  UTCI_Value = expand.grid(ta = ta, tr = tr, vel = vel, rh = rh, UTCI = utci_approx(ta, tr, vel, rh))
  c(min(UTCI_Value$UTCI), max(UTCI_Value$UTCI))
}
