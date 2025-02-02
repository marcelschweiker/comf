#' @noRd
#' @keywords internal
# help functions

#### function bisect ####
## used for bisection method in search for ta.adj for calculation of adjusted pmv including cooling effect of elevated air speed using set according to ASHRAE 55-2013
## code based on forum entry by ravi Varadhan rvaradhan at jhmi.edu
## https://stat.ethz.ch/pipermail/r-help/2010-september/253236.html

bisect <- function(fn, lower, upper, tol = 1.e-07, ...) {
  f.lo <- fn(lower, ...)
  f.hi <- fn(upper, ...)
  # feval <- 2

  if (f.lo * f.hi > 0) stop("Root is not bracketed in the specified interval \n")
  chg <- upper - lower

  while (abs(chg) > tol) {
    x.new <- (lower + upper) / 2
    f.new <- fn(x.new, ...)
    if (abs(f.new) <= tol) break
    if (f.lo * f.new < 0) upper <- x.new
    if (f.hi * f.new < 0) lower <- x.new
    chg <- upper - lower
    # feval <- feval + 1
  }
  list(x = x.new) # , value = f.new, fevals=feval)
}

#### An example
# fn1 <- function(x, a) {
# exp(-x) - a*x
# }

# bisect(fn1, 0, 2, a=1)

# bisect(fn1, 0, 2, a=2)


# function(ta, tr, vel, rh, clo=.5, met=1, wme=0){
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
### assignment of standard values for variables missing in input but required for calculation
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

###############################################
## definitions of necessary functions for HBx-calculation
###############################################

## mshiv  # program following function of hypothalamus
## Calculation of thermal-energy genration by shivering by stolwijk and Hardy
mshiv <- function(tcrSet, tskSet, tcr, tsk) {
  signalCr <- tcrSet - tcr
  signalCr <- ifelse(signalCr < 0, 0, signalCr)
  signalSk <- tskSet - tsk
  signalSk <- ifelse(signalSk < 0, 0, signalSk)
  mshivx <- 19.4 * signalCr * signalSk
  mshivx
}

## metaTherm: #Calculation of thermal energy emission rate taking the rate of external work into consideration
metaTherm <- function(met, basMet) {
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
QLoad <- function(met, pmv, basMet) {
  QLoadx <- pmv / (0.303 * exp(-0.036 * basMet * met))
  QLoadx
}

## tskReq #Required skin temperature for thermal-energy-wise neutral condition
## Fanger#s regression line based on the data collected by Rohles and Nevins
tskReq <- function(qmet) {
  tskReqx <- 35.7 - 0.028 * qmet
  tskReqx
}

## QperReq #Required resperation rate from the skin surface for thermal-energy-wise neutral condition
## Fanger#s regression line based on the data collected by Rohles and Nevins
QperReq <- function(qmet) {
  QQ <- 0.42 * (qmet - 58.15)
  QQ <- ifelse(QQ < 0, 0, QQ)
  QperReqx <- QQ
  QperReqx
}

## Qskin_evap: Thermal energy emission rate by water-vapour diffusion from the skin surface
Qskin_evap <- function(qmet, pa) {
  Qskin_evapx <- 3.05 * 10^(-3) * (5733. - 6.99 * qmet - pa)
  Qskin_evapx
}

##########################
## functions to get blood flow
##########################

## vbl: Calculation of skin blood flow. vbl is in the unit of "litre/(m2h)".
vbl <- function(tcrSet, tskSet, tcr, tsk) {
  signalCr <- tcr - tcrSet
  if (signalCr < 0) {
    signalCr <- 0
  }
  signalSk <- tskSet - tsk
  if (signalSk < 0) {
    signalSk <- 0
  }
  vblx <- (6.3 + 200 * signalCr) / (1 + 0.5 * signalSk)
  if (vblx < 0.5) {
    vblx <- 0.5
  }
  if (90 < vblx) {
    vblx <- 90
  }
  vblx
}

## vbl Variation 2: #Calculation of skin blood flow. vbl is in the unit of "litre/(m2h)".
vblCdilStr <- function(cdil, str1, tcrSet, tskSet, tcr, tsk) { # str1 changed from str due to being an R function
  signalCr <- tcr - tcrSet
  if (signalCr < 0) {
    signalCr <- 0
  }
  signalSk <- tskSet - tsk
  if (signalSk < 0) {
    signalSk <- 0
  }
  vblCdilStrx <- (6.3 + cdil * signalCr) / (1 + str1 * signalSk)
  if (vblCdilStrx < 0.5) {
    vblCdilStrx <- 0.5
  }
  if (90 < vblCdilStrx) {
    vblCdilStrx <- 90
  }
  vblCdilStrx
}

## mrsw1: Calculation of regulatory sweating rate. mrsw1 is in the unit of "mg/m2s".
mrsw1 <- function(tcrSet, tskSet, tcr, tsk) {
  signalCr <- tcr - tcrSet
  signalSk <- tskSet - tsk
  if (signalCr > 0) {
    signalCr <- 0
  }
  if (signalSk > 0) {
    signalSk <- 0
  }
  mrsw1x <- (250 * signalCr + 100 * signalCr * signalSk) * 2^(signalSk / 3) * (1000 / 3600)
  mrsw1x
}

## m: Calculation of regulatory sweating rate. ersw is in the unit of "mg/m2s".
mrsw2 <- function(tcrSet, tskSet, tcr, tsk, Qbl) {
  alpha <- 0.0418 + 0.745 / (Qbl + 0.585)
  tb <- (1 - alpha) * tcr + alpha * tsk
  tbSet <- (1 - alpha) * tcrSet + alpha * tskSet
  signalTb <- tb - tbSet
  if (signalTb > 0) {
    signalTb <- 0
  }
  mrsw2x <- 250 * signalTb * exp((tsk - tskSet) / 10.7) * (1000 / 3600)
  mrsw2x
}


## Calculation of convective heat transfer coefficient
## Equations by mitchell(1974) quoted from ASHRAE Handbook-Fundamentals-2005, p.8.8
hcvM <- function(v) {
  if (v <= 0.2) {
    hcvMx <- 3.1
  } else {
    hcvMx <- 8.3 * v^0.6
  }
  hcvMx
}

## Calculation of convective heat transfer coefficient
## Equations used by Fanger quoted from ASHRAE Handbook-Fundamentals-2005, p.8.16
hcvF <- function(v, tcl, ta) {
  hc1 <- 2.38 * abs(tcl - ta)^0.25
  hc2 <- 12.1 * sqrt(v)
  hcvFx <- hc1
  if (hc1 < hc2) {
    hcvFx <- hc2
  }
  hcvFx
}

## Calculation of convective heat transfer coefficient
## Equations used by Gagge et al.(1986)
hcvG <- function(va, met, basMet) {
  qmet <- met * basMet
  QQ <- qmet / basMet - 0.85
  if (QQ < 0) {
    hc1 <- 0
  } else {
    hc1 <- 5.66 * QQ^0.39
  }
  hc2 <- 8.6 * va^0.53
  hcvGx <- hc1
  if (hc1 < hc2) {
    hcvGx <- hc2
  }
  hcvGx
}

## Calculation of water vapor pressure in pa
pVapor <- function(tCel, phi) { # tCel original t
  phi_air <- phi / 100
  TK <- tCel + 273.15
  pVaporx <- phi_air * exp(25.89 - 5319 / TK)
  pVaporx
}

## Calculation of Effective temperature
calcet <- function(top, ta, pha, w, im, phaEt, imStar) {
  delta <- 0.1
  lr <- 16.5 * 10^(-3)
  tko <- 273.15
  C <- top + w * im * lr * 0.01 * pha * exp(25.89 - 5319 / (tko + ta))
  tes0 <- ta
  Y <- tes0 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) - C
  Z <- 1 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) * 5319 / ((tko + tes0)^2)
  tes1 <- tes0 - Y / Z
  while (abs(tes1 - tes0) > delta) {
    Y <- tes0 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) - C
    Z <- 1 + w * imStar * lr * 0.01 * phaEt * exp(25.89 - 5319 / (tko + tes0)) * 5319 / ((tko + tes0)^2)
    tes1 <- tes0 - Y / Z
    tes0 <- tes1
  }
  etx <- tes1
  etx
}

## Calculation of thermal exergy contained by a body with heat capacity of "cp"
wcEx <- function(cp, t1, too) {
  wcExx <- cp * ((t1 - too) - too * log(t1 / too))
  wcExx
}

## Judgement of exergy regarding "warm" or "cool"
wcXCheck <- function(t1, too) {
  if (t1 < too) {
    wcXCheckx <- "cool"
  } else {
    wcXCheckx <- "warm"
  }
  wcXCheckx
}

##  Calculation of material(wet/dry) exergy contained by one cubic-meter of moist air
wdEx <- function(t1, too, pv1, pvo) {
  pot <- 101325
  wdExx <- too / t1 * ((pot - pv1) * log((pot - pv1) / (pot - pvo)) + pv1 * log(pv1 / pvo))
  wdExx
}

##  Calculation of material(wet/dry) exergy contained by one kilogram of liquid water
wdExLw <- function(too, pvso, pv1, pvo) {
  rGas <- 8.31446
  mWater <- 18.015 * 0.001
  pot <- 101325
  wdExLwx <- rGas / (mWater) * too * (log(pvso / pvo) + (pot - pv1) / pv1 * log((pot - pv1) / (pot - pvo)))
  wdExLwx
}

## Judgement of exergy regarding "wet" or "dry"
wdXCheck <- function(p1, poo) {
  if (p1 < poo) {
    wdXCheckx <- "dry"
  } else {
    wdXCheckx <- "wet"
  }
  wdXCheckx
}

## Check whether package is working
pckgCheck <- function() {
  lsCond <- createCond()
  res <- calcComfInd(lsCond, request = "all")
  resSet <- c(-0.13, 5.37, 25, 24.22, 0.18, 0.21, 6.45, 40.48, 0.02, 0.22, 0.2, 23.32, 19.91, 21.92, 23.75, 0.52, 26.65, 0.63, -0.14, -0.12, 0.02, 0.03, -0.13, 2.27, 28.28, 24.9, 42.9, 10.3)
  print(table(round(res, 2) == resSet))
  print(table(round(calcPMVPPD(25, 20, .2, 50), 2) == c(-1.53, 52.75)))
}


########### help functions for solarGain

#' @keywords internal
solarGain <- function(solAlt, solAzi, solRadDir, solTrans,
                      fSvv, fBes, asw = 0.7,
                      posture = "seated", floorRef = 0.6) {
  degToRad <- 0.0174532925
  hr <- 6
  i_diff <- 0.2 * solRadDir

  fp_table <- rbind(
    c(0.35, 0.35, 0.314, 0.258, 0.206, 0.144, 0.082),
    c(0.342, 0.342, 0.31, 0.252, 0.2, 0.14, 0.082),
    c(0.33, 0.33, 0.3, 0.244, 0.19, 0.132, 0.082),
    c(0.31, 0.31, 0.275, 0.228, 0.175, 0.124, 0.082),
    c(0.283, 0.283, 0.251, 0.208, 0.16, 0.114, 0.082),
    c(0.252, 0.252, 0.228, 0.188, 0.15, 0.108, 0.082),
    c(0.23, 0.23, 0.214, 0.18, 0.148, 0.108, 0.082),
    c(0.242, 0.242, 0.222, 0.18, 0.153, 0.112, 0.082),
    c(0.274, 0.274, 0.245, 0.203, 0.165, 0.116, 0.082),
    c(0.304, 0.304, 0.27, 0.22, 0.174, 0.121, 0.082),
    c(0.328, 0.328, 0.29, 0.234, 0.183, 0.125, 0.082),
    c(0.344, 0.344, 0.304, 0.244, 0.19, 0.128, 0.082),
    c(0.347, 0.347, 0.308, 0.246, 0.191, 0.128, 0.082)
  )

  if (posture == "seated") {
    fp_table <- rbind(
      c(0.29, 0.324, 0.305, 0.303, 0.262, 0.224, 0.177),
      c(0.292, 0.328, 0.294, 0.288, 0.268, 0.227, 0.177),
      c(0.288, 0.332, 0.298, 0.29, 0.264, 0.222, 0.177),
      c(0.274, 0.326, 0.294, 0.289, 0.252, 0.214, 0.177),
      c(0.254, 0.308, 0.28, 0.276, 0.241, 0.202, 0.177),
      c(0.23, 0.282, 0.262, 0.26, 0.233, 0.193, 0.177),
      c(0.216, 0.26, 0.248, 0.244, 0.22, 0.186, 0.177),
      c(0.234, 0.258, 0.236, 0.227, 0.208, 0.18, 0.177),
      c(0.262, 0.26, 0.224, 0.208, 0.196, 0.176, 0.177),
      c(0.28, 0.26, 0.21, 0.192, 0.184, 0.17, 0.177),
      c(0.298, 0.256, 0.194, 0.174, 0.168, 0.168, 0.177),
      c(0.306, 0.25, 0.18, 0.156, 0.156, 0.166, 0.177),
      c(0.3, 0.24, 0.168, 0.152, 0.152, 0.164, 0.177)
    )
  }

  if (posture == "supine") {
    alt_temp <- solAlt
    solAlt <- abs(90 - solAzi)
    solAzi <- alt_temp
  }

  alt_range <- c(0, 15, 30, 45, 60, 75, 90)
  az_range <- c(0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180)
  alt_i <- findSpan(alt_range, solAlt)
  az_i <- findSpan(az_range, solAzi)
  fp11 <- fp_table[az_i, alt_i]
  fp12 <- fp_table[az_i, alt_i + 1]
  fp21 <- fp_table[az_i + 1, alt_i]
  fp22 <- fp_table[az_i + 1, alt_i + 1]
  az1 <- az_range[az_i]
  az2 <- az_range[az_i + 1]
  alt1 <- alt_range[alt_i]
  alt2 <- alt_range[alt_i + 1]
  fp <- fp11 * (az2 - solAzi) * (alt2 - solAlt)
  fp <- fp + (fp21 * (solAzi - az1) * (alt2 - solAlt))
  fp <- fp + (fp12 * (az2 - solAzi) * (solAlt - alt1))
  fp <- fp + (fp22 * (solAzi - az1) * (solAlt - alt1))
  fp <- fp / ((az2 - az1) * (alt2 - alt1))
  f_eff <- 0.725

  if (posture == "seated") {
    f_eff <- 0.696
  }

  sw_abs <- asw
  lw_abs <- 0.95

  e_diff <- f_eff * fSvv * 0.5 * solTrans * i_diff
  e_direct <- f_eff * fp * solTrans * fBes * solRadDir
  e_refl <- (
    f_eff
    * fSvv
      * 0.5
      * solTrans
      * (solRadDir * sin(solAlt * degToRad) + i_diff)
      * floorRef
  )
  e_solar <- e_diff + e_direct + e_refl
  erf <- e_solar * (sw_abs / lw_abs)
  delMrt <- erf / (hr * f_eff)
  return(c(round(erf, 1), round(delMrt, 1)))
}

#' @keywords internal
findSpan <- function(arr, x) {
  for (i in seq(1, length(arr) - 1)) {
    if (arr[i] <= x && arr[i + 1] >= x) {
      return(i)
    }
  }
  return(-1)
}

###### help functions for calcUTCI


# function for calculating UTCI Value
#' @keywords internal
utciApprox <- function(ta, tr, vel, rh) {
  ehPa <- es(ta) * (rh / 100.0)
  delta_t_tr <- tr - ta

  # convelert velapour pressure to kPa
  Pa <- ehPa / 10.0

  # calculation of the utci value
  utci <- (
    ta +
      (0.607562052)
      + (-0.0227712343) * ta
      + (8.06470249 * (10**(-4))) * ta * ta
      + (-1.54271372 * (10**(-4))) * ta * ta * ta
      + (-3.24651735 * (10**(-6))) * ta * ta * ta * ta
      + (7.32602852 * (10**(-8))) * ta * ta * ta * ta * ta
      + (1.35959073 * (10**(-9))) * ta * ta * ta * ta * ta * ta
      + (-2.25836520) * vel
      + (0.0880326035) * ta * vel
      + (0.00216844454) * ta * ta * vel
      + (-1.53347087 * (10**(-5))) * ta * ta * ta * vel
      + (-5.72983704 * (10**(-7))) * ta * ta * ta * ta * vel
      + (-2.55090145 * (10**(-9))) * ta * ta * ta * ta * ta * vel
      + (-0.751269505) * vel * vel
      + (-0.00408350271) * ta * vel * vel
      + (-5.21670675 * (10**(-5))) * ta * ta * vel * vel
      + (1.94544667 * (10**(-6))) * ta * ta * ta * vel * vel
      + (1.14099531 * (10**(-8))) * ta * ta * ta * ta * vel * vel
      + (0.158137256) * vel * vel * vel
      + (-6.57263143 * (10**(-5))) * ta * vel * vel * vel
      + (2.22697524 * (10**(-7))) * ta * ta * vel * vel * vel
      + (-4.16117031 * (10**(-8))) * ta * ta * ta * vel * vel * vel
      + (-0.0127762753) * vel * vel * vel * vel
      + (9.66891875 * (10**(-6))) * ta * vel * vel * vel * vel
      + (2.52785852 * (10**(-9))) * ta * ta * vel * vel * vel * vel
      + (4.56306672 * (10**(-4))) * vel * vel * vel * vel * vel
      + (-1.74202546 * (10**(-7))) * ta * vel * vel * vel * vel * vel
      + (-5.91491269 * (10**(-6))) * vel * vel * vel * vel * vel * vel
      + (0.398374029) * delta_t_tr
      + (1.83945314 * (10**(-4))) * ta * delta_t_tr
      + (-1.73754510 * (10**(-4))) * ta * ta * delta_t_tr
      + (-7.60781159 * (10**(-7))) * ta * ta * ta * delta_t_tr
      + (3.77830287 * (10**(-8))) * ta * ta * ta * ta * delta_t_tr
      + (5.43079673 * (10**(-10))) * ta * ta * ta * ta * ta * delta_t_tr
      + (-0.0200518269) * vel * delta_t_tr
      + (8.92859837 * (10**(-4))) * ta * vel * delta_t_tr
      + (3.45433048 * (10**(-6))) * ta * ta * vel * delta_t_tr
      + (-3.77925774 * (10**(-7))) * ta * ta * ta * vel * delta_t_tr
      + (-1.69699377 * (10**(-9))) * ta * ta * ta * ta * vel * delta_t_tr
      + (1.69992415 * (10**(-4))) * vel * vel * delta_t_tr
      + (-4.99204314 * (10**(-5))) * ta * vel * vel * delta_t_tr
      + (2.47417178 * (10**(-7))) * ta * ta * vel * vel * delta_t_tr
      + (1.07596466 * (10**(-8))) * ta * ta * ta * vel * vel * delta_t_tr
      + (8.49242932 * (10**(-5))) * vel * vel * vel * delta_t_tr
      + (1.35191328 * (10**(-6))) * ta * vel * vel * vel * delta_t_tr
      + (-6.21531254 * (10**(-9))) * ta * ta * vel * vel * vel * delta_t_tr
      + (-4.99410301 * (10**(-6))) * vel * vel * vel * vel * delta_t_tr
      + (-1.89489258 * (10**(-8))) * ta * vel * vel * vel * vel * delta_t_tr
      + (8.15300114 * (10**(-8))) * vel * vel * vel * vel * vel * delta_t_tr
      + (7.55043090 * (10**(-4))) * delta_t_tr * delta_t_tr
      + (-5.65095215 * (10**(-5))) * ta * delta_t_tr * delta_t_tr
      + (-4.52166564 * (10**(-7))) * ta * ta * delta_t_tr * delta_t_tr
      + (2.46688878 * (10**(-8))) * ta * ta * ta * delta_t_tr * delta_t_tr
      + (2.42674348 * (10**(-10))) * ta * ta * ta * ta * delta_t_tr * delta_t_tr
      + (1.54547250 * (10**(-4))) * vel * delta_t_tr * delta_t_tr
      + (5.24110970 * (10**(-6))) * ta * vel * delta_t_tr * delta_t_tr
      + (-8.75874982 * (10**(-8))) * ta * ta * vel * delta_t_tr * delta_t_tr
      + (-1.50743064 * (10**(-9))) * ta * ta * ta * vel * delta_t_tr * delta_t_tr
      + (-1.56236307 * (10**(-5))) * vel * vel * delta_t_tr * delta_t_tr
      + (-1.33895614 * (10**(-7))) * ta * vel * vel * delta_t_tr * delta_t_tr
      + (2.49709824 * (10**(-9))) * ta * ta * vel * vel * delta_t_tr * delta_t_tr
      + (6.51711721 * (10**(-7))) * vel * vel * vel * delta_t_tr * delta_t_tr
      + (1.94960053 * (10**(-9))) * ta * vel * vel * vel * delta_t_tr * delta_t_tr
      + (-1.00361113 * (10**(-8))) * vel * vel * vel * vel * delta_t_tr * delta_t_tr
      + (-1.21206673 * (10**(-5))) * delta_t_tr * delta_t_tr * delta_t_tr
      + (-2.18203660 * (10**(-7))) * ta * delta_t_tr * delta_t_tr * delta_t_tr
      + (7.51269482 * (10**(-9))) * ta * ta * delta_t_tr * delta_t_tr * delta_t_tr
      + (9.79063848 * (10**(-11)))
      * ta
        * ta
        * ta
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (1.25006734 * (10**(-6))) * vel * delta_t_tr * delta_t_tr * delta_t_tr
      + (-1.81584736 * (10**(-9))) * ta * vel * delta_t_tr * delta_t_tr * delta_t_tr
      + (-3.52197671 * (10**(-10)))
      * ta
        * ta
        * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (-3.36514630 * (10**(-8))) * vel * vel * delta_t_tr * delta_t_tr * delta_t_tr
      + (1.35908359 * (10**(-10)))
      * ta
        * vel
        * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (4.17032620 * (10**(-10)))
      * vel
        * vel
        * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (-1.30369025 * (10**(-9)))
      * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (4.13908461 * (10**(-10)))
      * ta
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (9.22652254 * (10**(-12)))
      * ta
        * ta
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (-5.08220384 * (10**(-9)))
      * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (-2.24730961 * (10**(-11)))
      * ta
        * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (1.17139133 * (10**(-10)))
      * vel
        * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (6.62154879 * (10**(-10)))
      * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (4.03863260 * (10**(-13)))
      * ta
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (1.95087203 * (10**(-12)))
      * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (-4.73602469 * (10**(-12)))
      * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
      + (5.12733497) * Pa
      + (-0.312788561) * ta * Pa
      + (-0.0196701861) * ta * ta * Pa
      + (9.99690870 * (10**(-4))) * ta * ta * ta * Pa
      + (9.51738512 * (10**(-6))) * ta * ta * ta * ta * Pa
      + (-4.66426341 * (10**(-7))) * ta * ta * ta * ta * ta * Pa
      + (0.548050612) * vel * Pa
      + (-0.00330552823) * ta * vel * Pa
      + (-0.00164119440) * ta * ta * vel * Pa
      + (-5.16670694 * (10**(-6))) * ta * ta * ta * vel * Pa
      + (9.52692432 * (10**(-7))) * ta * ta * ta * ta * vel * Pa
      + (-0.0429223622) * vel * vel * Pa
      + (0.00500845667) * ta * vel * vel * Pa
      + (1.00601257 * (10**(-6))) * ta * ta * vel * vel * Pa
      + (-1.81748644 * (10**(-6))) * ta * ta * ta * vel * vel * Pa
      + (-1.25813502 * (10**(-3))) * vel * vel * vel * Pa
      + (-1.79330391 * (10**(-4))) * ta * vel * vel * vel * Pa
      + (2.34994441 * (10**(-6))) * ta * ta * vel * vel * vel * Pa
      + (1.29735808 * (10**(-4))) * vel * vel * vel * vel * Pa
      + (1.29064870 * (10**(-6))) * ta * vel * vel * vel * vel * Pa
      + (-2.28558686 * (10**(-6))) * vel * vel * vel * vel * vel * Pa
      + (-0.0369476348) * delta_t_tr * Pa
      + (0.00162325322) * ta * delta_t_tr * Pa
      + (-3.14279680 * (10**(-5))) * ta * ta * delta_t_tr * Pa
      + (2.59835559 * (10**(-6))) * ta * ta * ta * delta_t_tr * Pa
      + (-4.77136523 * (10**(-8))) * ta * ta * ta * ta * delta_t_tr * Pa
      + (8.64203390 * (10**(-3))) * vel * delta_t_tr * Pa
      + (-6.87405181 * (10**(-4))) * ta * vel * delta_t_tr * Pa
      + (-9.13863872 * (10**(-6))) * ta * ta * vel * delta_t_tr * Pa
      + (5.15916806 * (10**(-7))) * ta * ta * ta * vel * delta_t_tr * Pa
      + (-3.59217476 * (10**(-5))) * vel * vel * delta_t_tr * Pa
      + (3.28696511 * (10**(-5))) * ta * vel * vel * delta_t_tr * Pa
      + (-7.10542454 * (10**(-7))) * ta * ta * vel * vel * delta_t_tr * Pa
      + (-1.24382300 * (10**(-5))) * vel * vel * vel * delta_t_tr * Pa
      + (-7.38584400 * (10**(-9))) * ta * vel * vel * vel * delta_t_tr * Pa
      + (2.20609296 * (10**(-7))) * vel * vel * vel * vel * delta_t_tr * Pa
      + (-7.32469180 * (10**(-4))) * delta_t_tr * delta_t_tr * Pa
      + (-1.87381964 * (10**(-5))) * ta * delta_t_tr * delta_t_tr * Pa
      + (4.80925239 * (10**(-6))) * ta * ta * delta_t_tr * delta_t_tr * Pa
      + (-8.75492040 * (10**(-8))) * ta * ta * ta * delta_t_tr * delta_t_tr * Pa
      + (2.77862930 * (10**(-5))) * vel * delta_t_tr * delta_t_tr * Pa
      + (-5.06004592 * (10**(-6))) * ta * vel * delta_t_tr * delta_t_tr * Pa
      + (1.14325367 * (10**(-7))) * ta * ta * vel * delta_t_tr * delta_t_tr * Pa
      + (2.53016723 * (10**(-6))) * vel * vel * delta_t_tr * delta_t_tr * Pa
      + (-1.72857035 * (10**(-8))) * ta * vel * vel * delta_t_tr * delta_t_tr * Pa
      + (-3.95079398 * (10**(-8))) * vel * vel * vel * delta_t_tr * delta_t_tr * Pa
      + (-3.59413173 * (10**(-7))) * delta_t_tr * delta_t_tr * delta_t_tr * Pa
      + (7.04388046 * (10**(-7))) * ta * delta_t_tr * delta_t_tr * delta_t_tr * Pa
      + (-1.89309167 * (10**(-8)))
      * ta
        * ta
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
      + (-4.79768731 * (10**(-7))) * vel * delta_t_tr * delta_t_tr * delta_t_tr * Pa
      + (7.96079978 * (10**(-9)))
      * ta
        * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
      + (1.62897058 * (10**(-9)))
      * vel
        * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
      + (3.94367674 * (10**(-8)))
      * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
      + (-1.18566247 * (10**(-9)))
      * ta
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
      + (3.34678041 * (10**(-10)))
      * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
      + (-1.15606447 * (10**(-10)))
      * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
      + (-2.80626406) * Pa * Pa
      + (0.548712484) * ta * Pa * Pa
      + (-0.00399428410) * ta * ta * Pa * Pa
      + (-9.54009191 * (10**(-4))) * ta * ta * ta * Pa * Pa
      + (1.93090978 * (10**(-5))) * ta * ta * ta * ta * Pa * Pa
      + (-0.308806365) * vel * Pa * Pa
      + (0.0116952364) * ta * vel * Pa * Pa
      + (4.95271903 * (10**(-4))) * ta * ta * vel * Pa * Pa
      + (-1.90710882 * (10**(-5))) * ta * ta * ta * vel * Pa * Pa
      + (0.00210787756) * vel * vel * Pa * Pa
      + (-6.98445738 * (10**(-4))) * ta * vel * vel * Pa * Pa
      + (2.30109073 * (10**(-5))) * ta * ta * vel * vel * Pa * Pa
      + (4.17856590 * (10**(-4))) * vel * vel * vel * Pa * Pa
      + (-1.27043871 * (10**(-5))) * ta * vel * vel * vel * Pa * Pa
      + (-3.04620472 * (10**(-6))) * vel * vel * vel * vel * Pa * Pa
      + (0.0514507424) * delta_t_tr * Pa * Pa
      + (-0.00432510997) * ta * delta_t_tr * Pa * Pa
      + (8.99281156 * (10**(-5))) * ta * ta * delta_t_tr * Pa * Pa
      + (-7.14663943 * (10**(-7))) * ta * ta * ta * delta_t_tr * Pa * Pa
      + (-2.66016305 * (10**(-4))) * vel * delta_t_tr * Pa * Pa
      + (2.63789586 * (10**(-4))) * ta * vel * delta_t_tr * Pa * Pa
      + (-7.01199003 * (10**(-6))) * ta * ta * vel * delta_t_tr * Pa * Pa
      + (-1.06823306 * (10**(-4))) * vel * vel * delta_t_tr * Pa * Pa
      + (3.61341136 * (10**(-6))) * ta * vel * vel * delta_t_tr * Pa * Pa
      + (2.29748967 * (10**(-7))) * vel * vel * vel * delta_t_tr * Pa * Pa
      + (3.04788893 * (10**(-4))) * delta_t_tr * delta_t_tr * Pa * Pa
      + (-6.42070836 * (10**(-5))) * ta * delta_t_tr * delta_t_tr * Pa * Pa
      + (1.16257971 * (10**(-6))) * ta * ta * delta_t_tr * delta_t_tr * Pa * Pa
      + (7.68023384 * (10**(-6))) * vel * delta_t_tr * delta_t_tr * Pa * Pa
      + (-5.47446896 * (10**(-7))) * ta * vel * delta_t_tr * delta_t_tr * Pa * Pa
      + (-3.59937910 * (10**(-8))) * vel * vel * delta_t_tr * delta_t_tr * Pa * Pa
      + (-4.36497725 * (10**(-6))) * delta_t_tr * delta_t_tr * delta_t_tr * Pa * Pa
      + (1.68737969 * (10**(-7)))
      * ta
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
      + (2.67489271 * (10**(-8)))
      * vel
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
      + (3.23926897 * (10**(-9)))
      * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
      + (-0.0353874123) * Pa * Pa * Pa
      + (-0.221201190) * ta * Pa * Pa * Pa
      + (0.0155126038) * ta * ta * Pa * Pa * Pa
      + (-2.63917279 * (10**(-4))) * ta * ta * ta * Pa * Pa * Pa
      + (0.0453433455) * vel * Pa * Pa * Pa
      + (-0.00432943862) * ta * vel * Pa * Pa * Pa
      + (1.45389826 * (10**(-4))) * ta * ta * vel * Pa * Pa * Pa
      + (2.17508610 * (10**(-4))) * vel * vel * Pa * Pa * Pa
      + (-6.66724702 * (10**(-5))) * ta * vel * vel * Pa * Pa * Pa
      + (3.33217140 * (10**(-5))) * vel * vel * vel * Pa * Pa * Pa
      + (-0.00226921615) * delta_t_tr * Pa * Pa * Pa
      + (3.80261982 * (10**(-4))) * ta * delta_t_tr * Pa * Pa * Pa
      + (-5.45314314 * (10**(-9))) * ta * ta * delta_t_tr * Pa * Pa * Pa
      + (-7.96355448 * (10**(-4))) * vel * delta_t_tr * Pa * Pa * Pa
      + (2.53458034 * (10**(-5))) * ta * vel * delta_t_tr * Pa * Pa * Pa
      + (-6.31223658 * (10**(-6))) * vel * vel * delta_t_tr * Pa * Pa * Pa
      + (3.02122035 * (10**(-4))) * delta_t_tr * delta_t_tr * Pa * Pa * Pa
      + (-4.77403547 * (10**(-6))) * ta * delta_t_tr * delta_t_tr * Pa * Pa * Pa
      + (1.73825715 * (10**(-6))) * vel * delta_t_tr * delta_t_tr * Pa * Pa * Pa
      + (-4.09087898 * (10**(-7)))
      * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
        * Pa
      + (0.614155345) * Pa * Pa * Pa * Pa
      + (-0.0616755931) * ta * Pa * Pa * Pa * Pa
      + (0.00133374846) * ta * ta * Pa * Pa * Pa * Pa
      + (0.00355375387) * vel * Pa * Pa * Pa * Pa
      + (-5.13027851 * (10**(-4))) * ta * vel * Pa * Pa * Pa * Pa
      + (1.02449757 * (10**(-4))) * vel * vel * Pa * Pa * Pa * Pa
      + (-0.00148526421) * delta_t_tr * Pa * Pa * Pa * Pa
      + (-4.11469183 * (10**(-5))) * ta * delta_t_tr * Pa * Pa * Pa * Pa
      + (-6.80434415 * (10**(-6))) * vel * delta_t_tr * Pa * Pa * Pa * Pa
      + (-9.77675906 * (10**(-6))) * delta_t_tr * delta_t_tr * Pa * Pa * Pa * Pa
      + (0.0882773108) * Pa * Pa * Pa * Pa * Pa
      + (-0.00301859306) * ta * Pa * Pa * Pa * Pa * Pa
      + (0.00104452989) * vel * Pa * Pa * Pa * Pa * Pa
      + (2.47090539 * (10**(-4))) * delta_t_tr * Pa * Pa * Pa * Pa * Pa
      + (0.00148348065) * Pa * Pa * Pa * Pa * Pa * Pa
  )
  round(utci, 1)
}

# function for calculating the es value
#' @keywords internal
es <- function(ta) {
  g <- c(
    -2836.5744,
    -6028.076559,
    19.54263612,
    -0.02737830188,
    0.000016261698,
    (7.0229056 * (10**(-10))),
    (-1.8680009 * (10**(-13)))
  )
  # converting air temparature in K
  tk <- ta + 273.15
  es <- 2.7150305 * log1p(tk)
  for (i in seq_along(g)) {
    es <- es + (g[i] * (tk**(i - 3)))
  }
  # converting to hPa
  es <- exp(es) * 0.01
  es
}


# utils for ATHBPMV
#' @noRd

calcCoolingStrategyBuilding <- function(coolingStrategyBuilding) {
  coolingStrategyBuildingValue <- tolower(gsub(" ", "", coolingStrategyBuilding))
  return(ifelse(c("mixedmode", "naturallyventilated") == coolingStrategyBuildingValue, 1, 0))
}

calcBuildingType <- function(buildingType) {
  buildingTypeValue <- tolower(gsub(" ", "", buildingType))
  multiFamilyHousing <- if (buildingType == "multiFamilyHousing") 1 else 0
  office <- if (buildingType == "office") 1 else 0
  others <- if (buildingType == "others") 1 else 0
  return(c(multiFamilyHousing, office, others))
}

calcBuildingTypeSimple <- function(buildingTypeSimple) {
  buildingTypeSimpleValue <- tolower(gsub(" ", "", buildingTypeSimple))
  return(ifelse(c("multiFamilyHousing", "office") == buildingTypeSimpleValue, 1, 0))
}

calcSeason <- function(season) {
  seasonValue <- tolower(gsub(" ", "", season))
  spring <- if (seasonValue == "spring") 1 else 0
  summer <- if (seasonValue == "summer") 1 else 0
  winter <- if (seasonValue == "winter") 1 else 0
  return(c(spring, summer, winter))
}

#' @keywords internal
p_sat <- function(tdb) {
  # Constants for the equation
  c_to_k <- 273.15
  ta_k <- tdb + c_to_k

  c1 <- -5674.5359
  c2 <- 6.3925247
  c3 <- -0.9677843 * 1e-2
  c4 <- 0.62215701 * 1e-6
  c5 <- 0.20747825 * 1e-8
  c6 <- -0.9484024 * 1e-12
  c7 <- 4.1635019
  c8 <- -5800.2206
  c9 <- 1.3914993
  c10 <- -0.048640239
  c11 <- 0.41764768 * 1e-4
  c12 <- -0.14452093 * 1e-7
  c13 <- 6.5459673

  # Calculate pascals depending on temperature
  pascals <- ifelse(
    ta_k < c_to_k,
    exp(
      c1 / ta_k +
        c2 +
        ta_k * (c3 + ta_k * (c4 + ta_k * (c5 + c6 * ta_k))) +
        c7 * log(ta_k)
    ),
    exp(
      c8 / ta_k +
        c9 +
        ta_k * (c10 + ta_k * (c11 + ta_k * c12)) +
        c13 * log(ta_k)
    )
  )

  # Round the results
  return(round(pascals, 1))
}
