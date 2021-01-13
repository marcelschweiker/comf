cToK = 273.15
cpVapour = 1805.0
cpWater = 4186
cpAir = 1004
hFg = 2501000
rAir = 287.055

#Estimates the saturation vapor pressure in [torr]
pSatTorr <- function(tdb){
  exp(18.6686 - 4030.183 / (tdb + 235.0))
}

#Estimates the relative air velocity which combines the average air velocity of 
#the space plus the relative air velocity caused by the body movement.
vRelative <- function(vel, met){
  vr = ifelse(met > 1, round(vel + 0.3 * (met - 1), 3), vel)
  vr
}

#Estimates the dynamic clothing insulation of a moving occupant. The activity as
#well as the air speed modify the insulation characteristics of the clothing and 
#the adjacent air layer. Consequently the ISO 7730 states that the clothing insulation
#shall be corrected [2]_. The ASHRAE 55 Standard, instead, only corrects for the effect
#of the body movement, and states that the correction is permitted but not required.
cloDynamic <- function(clo, met, standard = "ASHARE"){
  if(!tolower(standard) %in% c("ashare")){
    stop("PMV calculations can only be performed in compliance with ISO or ASHRAE Standards")
  }
  
  cd = ifelse(1.2 < met & met< 2, round(clo * (0.6 + 0.4 / met), 3), clo)
  cd
} 

# Estimates the running mean temperature
runningMeanOutdoorTemperature <- function(tempArray, alpha=0.8, units="SI") {
  if(tolower(units) == "ip"){
    for(i in tempArray){
      temp_array[ix] = units_converter(tdb=temp_array[ix])[1]
    }
  }
  coeff = 0:(length(tempArray)-1)
  trm = 0
  
  for(i in 1:length(coeff)){
    coeff[i] = alpha^coeff[i]
    trm = trm + (coeff[i] * tempArray[i])
  }
  trm = trm/sum(coeff)
  if(tolower(units) == "ip"){
    trm = units_converter(tmp=trm, fromUnits="si")[1]
  }
  round(trm,1)
}

# Converts IP values to SI units
unitsConverter <- function(from_units="ip", ...){
  kwargs = list(...)
  results = c()
  
  if(from_units == "ip"){
    for(i in 1:length(kwargs)){
      key = names(kwargs[i])
      if(grepl("tmp", key) || key == "tr" || key == "tdb")  {
        results <- c(results, round((kwargs[[i]] - 32) * 5 / 9,2))
      }
      if(key %in% c("v", "vr", "vel")) results <- c(results, round(kwargs[[i]] / 3.281, 2))
      if(key == "area") results <- c(results, round(kwargs[[i]] / 10.764,2))
      if(key == "pressure") results <- c(results, round(kwargs[[i]] * 101325,2))
    }
  }
  else if(from_units == "si"){
    for(i in 1:length(kwargs)){
      key = names(kwargs[i])
      if(grepl("tmp", key) || key == "tr" || key == "tdb")  {
        results <- c(results, (kwargs[[i]] * 9 / 5) + 32)
      }
      if(key %in% c("v", "vr", "vel")) results <- c(results, kwargs[[i]] * 3.281)
      if(key == "area") results <- c(results, kwargs[[i]] * 10.764)
      if(key == "pressure") results <- c(results, kwargs[[i]] / 101325)
    }
  }
  results
}

# Calculates operative temperature in accordance with ISO 7726:1998 [5]_
to <- function(tdb, tr, vel){
  (tdb * sqrt(10 * vel) + tr) / (1 + sqrt(10 * vel))
}

# Calculates air enthalpy
enthalpy <- function(tdb, hr){
  hDryAir = cpAir * tdb
  hSatVap = hFg + cpVapour * tdb
  h = hDryAir + hr * hSatVap
  round(h, 2)
}

# Calculates vapour pressure of water at different temperatures
pSat <- function(tdb){
  taK = tdb + cToK
  c1 = -5674.5359
  c2 = 6.3925247
  c3 = -0.9677843 * 10^-2
  c4 = 0.62215701 * 10^-6
  c5 = 0.20747825 * 10^-8
  c6 = -0.9484024 * 10^-12
  c7 = 4.1635019
  c8 = -5800.2206
  c9 = 1.3914993
  c10 = -0.048640239
  c11 = 0.41764768 * 10^-4
  c12 = -0.14452093 * 10^-7
  c13 = 6.5459673
  
  if(taK < cToK){
    round(exp(
      c1 / taK
      + c2
      + taK * (c3 + taK * (c4 + taK * (c5 + c6 * taK)))
      + c7 * log(taK)
    ),1)
  }
  else{
    round(exp(
      c8 / taK
      + c9
      + taK * (c10 + taK * (c11 + taK * c12))
      + c13 * log(taK)
    ),1)
  }
}

# Calculates psychrometric values of air based on dry bulb air temperature and 
# relative humidity
psyTaRh <- function(tdb, rh, patm=101325){
  psat = pSat(tdb)
  pvap = rh / 100 * psat
  hr = 0.62198 * pvap / (patm - pvap)
  tdp = tDp(tdb, rh)
  twb = tWb(tdb, rh)
  h = enthalpy(tdb, hr)
  
  list("pSat"= psat, "pVap"= pvap, "hr"= hr, "tWb"= twb, "tDp"= tdp, "h"= h)
}

# Calculates the wet-bulb temperature using the Stull equation [6]_
tWb <- function(tdb, rh){
  round(
    tdb * atan(0.151977 * (rh + 8.313659) ** (1 / 2))
    + atan(tdb + rh)
    - atan(rh - 1.676331)
    + 0.00391838 * rh ** (3 / 2) * atan(0.023101 * rh)
    - 4.686035,
    1)
}

# Calculates the dew point temperature
tDp <- function(tdb, rh){
  c = 257.14
  b = 18.678
  a = 6.1121
  d = 234.5
  gammaM = log(rh / 100 * exp((b - tdb / d) * (tdb / (c + tdb))))
  round(c * gammaM / (b - gammaM), 1)
}

# Converts globe temperature reading into mean radiant temperature in accordance
# with ISO 7726:1998 [5]_
tMrt <- function(tg, tdb, v, d=0.15, emissivity=0.9){
  tg = tg + cToK
  tdb = tdb + cToK
  
  # calculate heat transfer coefficient
  hn = 1.4 * (abs(tg - tdb) / d) ^ 0.25  # natural convection
  hf = 6.3 * v ^ 0.6 / d ^ 0.4  # forced convection
  
  # get the biggest between the tow coefficients
  h = max(hf, hn)
  
  round((tg ^ 4 + h * (tg - tdb) / (emissivity * (5.67 * 10 ^ -8))) ^ 0.25 - cToK,1)
}
