#' @keywords internal
# help functions

utils::globalVariables(c("solarGainRange", "utciRange"))


#### function bisect ####
## used for bisection method in search for ta.adj for calculation of adjusted pmv including cooling effect of elevaated air speed using set according to ASHRAE 55-2013
## code based on forum entry by ravi Varadhan rvaradhan at jhmi.edu 
## https://stat.ethz.ch/pipermail/r-help/2010-september/253236.html

bisect <- function(fn, lower, upper, tol=1.e-07, ...) {
  f.lo <- fn(lower, ...) 
  f.hi <- fn(upper, ...) 
  #feval <- 2
  
  if (f.lo * f.hi > 0) stop("Root is not bracketed in the specified interval \n")
  chg <- upper - lower
  
  while (abs(chg) > tol) {
    x.new <- (lower + upper) / 2
    f.new <- fn(x.new, ...)
    if (abs(f.new) <= tol) break
    if (f.lo * f.new < 0) upper <- x.new 
    if (f.hi * f.new < 0) lower <- x.new 
    chg <- upper - lower
    #feval <- feval + 1
  }
  list(x = x.new)#, value = f.new, fevals=feval)
}

#### An example
# fn1 <- function(x, a) {
# exp(-x) - a*x 
# }

# bisect(fn1, 0, 2, a=1)

# bisect(fn1, 0, 2, a=2)


#function(ta, tr, vel, rh, clo=.5, met=1, wme=0){
# ta <- c(21:25)
# tr <- c(21:25)
# vel<- rep(.1, 5)
# rh<- rep(50, 5)
# asv <- c(-1, 0, 1, 1.5, 1)
# dfTest <- data.frame(ta, tr, vel, rh, asv)
# lsTest <- as.list(dfTest)


# calcAssignstrdVal <- function(nameslsCond, l, met, wme, pb, ltime, rh, clo, ta, tr, vel, ht, wt, tmmo, trm, tu){
# params <- c("met", "wme", "pb", "ltime", "rh", "clo", "ta", "tr", "vel", "ht", "wt", "tmmo", "trm", "tu")
# Vals<-c(1, 0, 760, 60, 50, 0.5, 25, 25, 0.1, 170, 70, 15, 15, 40)

# for (j in 1:length(params)){ 
###assignment of standard values for variables missing in input but required for calculation
# if((!params[j] %in% nameslsCond) | (NA %in% get(params[j])) | (length(get(params[j]))==0)){
# assign(params[j], rep.int(Vals[j], l))
# print(paste("warning! ", params[j], " is necessary for one or more of the indices required, but was not given in input data. For the calculation it was set to the standard value of ", Vals[j], " in all rows.", sep = ""))
# }else if(length(get(params[j])==1)){
# assign(params[j], rep.int(get(params[j]), l))
# }else if(length(get(params[j])!=l)){
# print(paste("error: Length of", params[j], "does not match!", sep=""))
# }
# }
# }


cutTSV <- function(pred){
  cut(pred, breaks = c(-300, -2.5, -1.5, -.5, .5, 1.5, 2.5, 300), labels = c("-3", "-2", "-1", "0", "1", "2", "3"))
}


# percentage of fit ThermalsensationSQ001 to pmv
calcTPRTSV <- function(ref, pred){
  (table(ref, pred)[1, 1] + table(ref, pred)[2, 2] + table(ref, pred)[3, 3] + table(ref, pred)[4, 4] + table(ref, pred)[5, 5] + table(ref, pred)[6, 6] + table(ref, pred)[7, 7]) / sum(table(ref, pred))
}




###############################################
## definitions of necessary functions for HBx-calculation
###############################################

## mshiv  # program following function of hypothalamus
## Calculation of thermal-energy genration by shivering by stolwijk and Hardy
mshiv <-function(tcrSet, tskSet, tcr, tsk){
  signalCr <- tcrSet - tcr; 
  signalCr <- ifelse (signalCr < 0, 0, signalCr)
  signalSk <- tskSet - tsk; 
  signalSk <- ifelse (signalSk < 0, 0, signalSk)
  mshivx <- 19.4 * signalCr * signalSk
  mshivx
}

## metaTherm: #Calculation of thermal energy emission rate taking the rate of external work into consideration
metaTherm <- function(met, basMet){
  eff <- 0
  if (met >= 1.4 & met < 3) {
    eff <- 0.1
  } else if (met >= 3) {
    eff <- 0.2
  }
  metaThermx <- basMet * met * (1 - eff)
  metaThermx
}

## QLoad: #heat storage rate of human body by Fanger#s equation
QLoad <- function(met, pmv, basMet){
  QLoadx <- pmv / (0.303 * exp(-0.036 * basMet * met))
  QLoadx
}

##tskReq #Required skin temperature for thermal-energy-wise neutral condition
##Fanger#s regression line based on the data collected by Rohles and Nevins
tskReq <- function(qmet){	
  tskReqx <- 35.7 - 0.028 * qmet
  tskReqx
}

## QperReq #Required resperation rate from the skin surface for thermal-energy-wise neutral condition
## Fanger#s regression line based on the data collected by Rohles and Nevins
QperReq <- function(qmet){
  QQ <- 0.42 * (qmet - 58.15)
  QQ <- ifelse (QQ < 0, 0, QQ)
  QperReqx <- QQ
  QperReqx
}

## Qskin_evap: Thermal energy emission rate by water-vapour diffusion from the skin surface
Qskin_evap <- function(qmet, pa){
  Qskin_evapx <- 3.05 * 10  ^  (-3) * (5733. - 6.99 * qmet - pa)
  Qskin_evapx
}

##########################
## functions to get blood flow
##########################

## vbl: Calculation of skin blood flow. vbl is in the unit of "litre/(m2h)".
vbl <- function(tcrSet, tskSet, tcr, tsk){
  signalCr <- tcr - tcrSet
  if (signalCr < 0){
    signalCr <- 0
  }
  signalSk <- tskSet - tsk
  if (signalSk < 0){
    signalSk <- 0
  }
  vblx <- (6.3 + 200 * signalCr) / (1 + 0.5 * signalSk)
  if (vblx < 0.5){
    vblx <- 0.5
  }
  if (90 < vblx) {
    vblx <- 90
  }
  vblx
}

## vbl Variation 2: #Calculation of skin blood flow. vbl is in the unit of "litre/(m2h)".
vblCdilStr <- function(cdil, str1, tcrSet, tskSet, tcr, tsk){ # str1 changed from str due to being an R function
  signalCr <- tcr - tcrSet
  if (signalCr < 0){
    signalCr <- 0
  }
  signalSk <- tskSet - tsk
  if (signalSk < 0){
    signalSk <- 0
  } 
  vblCdilStrx <- (6.3 + cdil * signalCr) / (1 + str1 * signalSk)
  if (vblCdilStrx < 0.5) {
    vblCdilStrx <- 0.5
  }
  if (90 < vblCdilStrx){
    vblCdilStrx <- 90
  }
  vblCdilStrx
}

## mrsw1: Calculation of regulatory sweating rate. mrsw1 is in the unit of "mg/m2s".
mrsw1 <- function(tcrSet, tskSet, tcr, tsk){
  signalCr <- tcr - tcrSet
  signalSk <- tskSet - tsk
  if (signalCr > 0){ 
    signalCr <- 0
  }
  if (signalSk > 0){ 
    signalSk <- 0
  } 
  mrsw1x <- (250 * signalCr + 100 * signalCr * signalSk) * 2  ^  (signalSk / 3) * (1000 / 3600)
  mrsw1x
}

## m: Calculation of regulatory sweating rate. ersw is in the unit of "mg/m2s".
mrsw2 <- function(tcrSet, tskSet, tcr, tsk, Qbl){
  alpha <- 0.0418 + 0.745 / (Qbl + 0.585)
  tb <- (1 - alpha) * tcr + alpha * tsk
  tbSet <- (1 - alpha) * tcrSet + alpha * tskSet
  signalTb <- tb - tbSet
  if(signalTb > 0){
    signalTb <- 0
  } 
  mrsw2x <- 250 * signalTb * exp((tsk - tskSet) / 10.7) * (1000 / 3600)
  mrsw2x
}


## Calculation of convective heat transfer coefficient
## Equations by mitchell(1974) quoted from ASHRAE Handbook-Fundamentals-2005, p.8.8
hcvM <- function(v){
  if (v <= 0.2){
    hcvMx <- 3.1
  } else {
    hcvMx <- 8.3 * v  ^  0.6
  }
  hcvMx 
}

## Calculation of convective heat transfer coefficient
## Equations used by Fanger quoted from ASHRAE Handbook-Fundamentals-2005, p.8.16
hcvF <- function(v, tcl, ta){
  hc1 <- 2.38 * abs(tcl - ta)  ^  0.25
  hc2 <- 12.1 * sqrt(v)
  hcvFx <- hc1
  if (hc1 < hc2){
    hcvFx <- hc2
  }
  hcvFx
}

## Calculation of convective heat transfer coefficient
## Equations used by Gagge et al.(1986)
hcvG <- function(va, met, basMet){
  qmet <- met * basMet
  QQ <- qmet / basMet - 0.85
  if (QQ < 0){
    hc1 <- 0
  } else {
    hc1 <- 5.66 * QQ  ^  0.39
  }
  hc2 <- 8.6 * va  ^  0.53
  hcvGx <- hc1
  if (hc1 < hc2){
    hcvGx <- hc2
  }
  hcvGx
}

## Calculation of water vapor pressure in pa
pVapor <- function(tCel, phi){ # tCel original t
  phi_air <- phi / 100
  TK <- tCel + 273.15
  pVaporx <- phi_air * exp(25.89 - 5319 / TK)
  pVaporx
}

## Calculation of Effective temperature
calcet <- function(top, ta, pha, w, im, phaEt, imStar){
  delta <- 0.1; lr <- 16.5 * 10  ^  (-3); tko <- 273.15
  C <- top + w * im * lr * 0.01 * pha * exp(25.89 - 5319 / (tko + ta))
  tes0 <- ta
  Y <- tes0 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) - C
  Z <- 1 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) * 5319 / ((tko + tes0)  ^  2)
  tes1 <- tes0 - Y / Z
  while (abs(tes1 - tes0) > delta){
    Y <- tes0 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) - C
    Z <- 1 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) * 5319 / ((tko + tes0)  ^  2)
    tes1 <- tes0 - Y / Z
    tes0 <- tes1
  }
  etx <- tes1
  etx
}

## Calculation of thermal exergy contained by a body with heat capacity of "cp"
wcEx <- function(cp, t1, too){
  wcExx <- cp * ((t1 - too) - too * log(t1 / too))
  wcExx
}

## Judgement of exergy regarding "warm" or "cool"
wcXCheck <- function(t1, too){
  if (t1 < too){
    wcXCheckx <- "cool"
  } else {
    wcXCheckx <- "warm"
  }
  wcXCheckx
}

##  Calculation of material(wet/dry) exergy contained by one cubic-meter of moist air
wdEx <- function(t1, too, pv1, pvo){
  pot <- 101325
  wdExx <- too / t1 * ((pot - pv1) * log((pot - pv1) / (pot - pvo)) + pv1 * log(pv1 / pvo))
  wdExx
}

##  Calculation of material(wet/dry) exergy contained by one kilogram of liquid water
wdExLw <- function(too, pvso, pv1, pvo){
  rGas <- 8.31446
  mWater <- 18.015 * 0.001
  pot <- 101325
  wdExLwx <- rGas / (mWater) * too * (log(pvso / pvo) + (pot - pv1) / pv1 * log((pot - pv1) / (pot - pvo)))
  wdExLwx
}

## Judgement of exergy regarding "wet" or "dry"
wdXCheck <- function(p1, poo){
  if (p1 < poo){
    wdXCheckx <- "dry"
  } else {
    wdXCheckx <- "wet"
  }
  wdXCheckx
}

## Check whether package is working
pckgCheck <- function(){
  lsCond <- createCond()
  res <- calcComfInd(lsCond, request = "all")
  resSet <- c(-0.13, 5.37, 25, 24.22, 0.18, 0.21, 6.45, 40.48, 0.02, 0.22, 0.2, 23.32, 19.91, 21.92, 23.75, 0.52, 26.65, 0.63, -0.14, -0.12, 0.02, 0.03, -0.13, 2.27, 28.28, 24.9, 42.9, 10.3)
  print(table(round(res, 2) == resSet))
  print(table(round(calcPMVPPD(25, 20, .2, 50), 2) == c(-1.53, 52.75)))
}
