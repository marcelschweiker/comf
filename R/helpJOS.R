#######################################################
#run all these functions before running JOS3-functions
#######################################################

#All Equations
calcDubois <- function(height,weight){
  return(0.2025 * (height ^ 0.725) * (weight ^ 0.425))
}
calcTakahira <- function(height,weight){
  return(0.2042 * (height ^ 0.725) * (weight ^ 0.425))
}
calcFujimoto <- function(height,weight){
  return(0.1882 * (height ^ 0.663) * (weight ^ 0.444))
}
calcKurazumi <- function(height,weight){
  return(0.2440 * (height ^ 0.693) * (weight ^ 0.383))
}
# Antoine equation [kPa]
calcAntoine <- function(x){
  return(exp(16.6536-(4030.183/(x+235))))
}
# Tetens equation [kPa]
calcTetens <- function(x){
  return(0.61078*10**(7.5*x/(x+237.3)))
}

###############################################

BSAst <- function(){
  c(0.110, 0.029, 0.175, 0.161, 0.221,0.096, 0.063, 0.050, 0.096, 0.063, 0.050,
    0.209, 0.112, 0.056, 0.209, 0.112, 0.056)
}

###############################################

calcWeightrate <- function(weight=74.43){

  rate <- weight / 74.43
  return(rate)
}

##############################################

calcfixedHC <- function(hc, va){
mean_hc <- weighted.mean(hc, weights=BSAst())
mean_va <- weighted.mean(va, weights=BSAst())
mean_hc_whole <- max(3, 8.600001*(mean_va**0.53))
fixed_hc <- hc * mean_hc_whole/mean_hc
return(fixed_hc)
}

##############################################

calcfixedHR <- function(hr){
mean_hr <- weighted.mean(hr, weights=BSAst())
fixed_hr <- hr * 4.7/mean_hr
return(fixed_hr)
}

##############################################

calcoperativeTEMP <- function(ta, tr, hc, hr){
otemp <- (hc*ta + hr*tr) / (hc + hr)
return(otemp)
}

##############################################

calcCLOarea <- function(clo){
if (clo<0.5){
  fcl <- clo*0.2+1
}
else { fcl <- clo*0.1+1.05
}
return(fcl)
}

##############################################

calcdryR <- function(hc, hr, clo){
  
  fcl <- calcCLOarea(clo)
  ra <- 1/(hc+hr)
  rcl <- 0.155*clo
  rt <- ra/fcl + rcl
  return(rt)
}

#############################################

calcwetR <- function(hc, clo, iclo=0.45, lewis_rate=16.5){
 
fcl = calcCLOarea(clo)
rcl = 0.155 * clo
rea = 1 / (lewis_rate * hc)
recl = rcl / (lewis_rate * iclo)
ret = rea / fcl + recl
return(ret)
}

#############################################
