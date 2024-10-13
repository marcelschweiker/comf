# Function: Two Node model ################
###########################################
#' Comfort Indices based on the 2-Node-Model
#'
#' @aliases calc2Node 2Node 2node
#' @description 
#' \code{calc2Node} calculates Comfort Indices based on the 2-Node-Model by Gagge et al.
#' 
#' @usage 
#' calc2Node(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, sa = NULL, pb = 760, 
#' ltime = 60, ht = 171, wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, 
#' cstr = 0.5, varOut = "else", bodyPosition = 'sitting')
#' 
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo] 
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @param sa (optional)surface Area according to mosteller formula [m^2]
#' @param pb a numeric value presenting barometric pressure in [torr] or [mmHg]
#' @param ltime a numeric value presenting exposure time in [minutes]
#' @param ht a numeric value presenting body height in [cm]
#' @param wt a numeric value presenting body weight in [kg]
#' @param tu a numeric value presenting turbulence intensity in [\%]
#' @param obj a character element, either "set" or "pmvadj"
#' @param csw a numeric value presenting the driving coefficient for regulatory sweating
#' @param cdil a numeric value presenting the driving coefficient for vasodilation
#' @param cstr a numeric value presenting the driving coefficient for vasoconstriction
#' @param varOut a string value either "else" for normal output of SET or "skinWet" to report value of skin wettedness
#' @param bodyPosition a string representing body position, has to be 'sitting' or 'standing'. Default value is 'sitting' 
#'
#' @details 
#' All variables must have the same length 1. For the calculation of several 
#' values use function \code{calcComfInd}. The value of \code{obj} defines 
#' whether the function will use the version presented in ASHRAE 55-2013 for 
#' adjustment of pmv (obj = "pmvadj"), or the original code by Gagge to calculate 
#' set (obj = "set"). In the version presented in ASHRAE 55-2013, the lines of 
#' code related to self-generated convection is deleted. Therefore, a difference 
#' can only be seen at higher values of met.
#' 
#' @returns returns a data.frame with the following items:
#' \describe{
#' \item{\code{et - }Effective temperature}{}
#' \item{\code{tsens - }Predicted thermal sensation}{}
#' \item{\code{disc - }Predicted discomfort}{}
#' \item{\code{ps - }Predicted percentage satisfied with the level of air movement}{}
#' \item{\code{pd - }Predicted percentage dissatisfied due to draft}{}
#' \item{\code{pts - }Predicted thermal sensation vote based on set}{}
#' \item{\code{pmvg - }Gagge's version of Fanger's PMV}{}
#' \item{\code{pmvstar - }Same as Fanger's PMV except that dry is calculated 
#' using SET* rather than the operative temperature}{}
#' }
#' The other functions return a single index, e.g. code(calcSET) returns the 
#' standard effective temperature.
#' 
#' @note 
#' In case one of the variables is not given, a standard value will be taken 
#' from a list (see \code{\link{createCond}} for details.
#' 
#' @author 
#' The code for \code{calc2Node} is based on the code in BASIC and C++ presented 
#' by Fountain and Huizenga (1995). The translation into R-language and comparison 
#' with ASHRAE 55-2013 conducted by Marcel Schweiker.
#' 
#' @references 
#' ASHRAE Standard 55-2013. Thermal environmental conditions for human occupancy. 
#' American society of heating, Refrigerating and Air-Conditioning Engineering, 
#' Atlanta, USA, 2013.
#' 
#' Fountain & Huizenga (1995) A thermal sensation model for use by the 
#' engineering profession ASHRAE RP-781 Final report.
#'
#' Gagge, Fobelets & Berglund (1986) A standard predictive index 
#' of human response to the thermal environment, ASHRAE transactions, 92 (2B), 709-731.
#' 
#' @seealso see also \code{\link{calcComfInd}}
#' @export
#'
#' @examples
#' ## Calculation of a single set of values.
#' calc2Node(22, 25, .50, 50)

calc2Node <- function(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, sa = NULL, pb = 760, 
                      ltime = 60, ht = 171, wt = 70, tu = 40, obj = "set", 
                      csw = 170, cdil = 120, cstr = .5, varOut="else", 
                      bodyPosition = 'sitting'){
  
  if(sa == 0 || is.null(sa)){
    sa    <- ((ht * wt) / 3600 ) ^ .5 # surface Area (m2) according to mosteller formula 
  }
  m <- met * 58.2 #[w/m2]
  w <- wme * 58.2 #[w/m2]
  mw <- m - w 
  
  kclo <- .25	
  csw  <- csw   #driving coefficient for regulatory sweating
  cdil <- cdil   #driving coefficient for vasodilation
  cstr <- cstr    #driving coefficient for vasoconstriction
  
  tskn  <- 33.7  #setpoint (neutral) value for tsk
  tcrn  <- 36.8  #setpoint value for tcr
  tbn   <- 36.49 #setpoint for tb (.1*tskn + .9*tcrn)
  skbfn <- 6.3   #neutral value for skbf
  sbc   <- 5.6697 * 10 ^ (-08) #stephan-Boltzmann constant
  
  vel <- max(vel, 0.1) # set minimum va to .1 m/s
  
  # INITIAL VALUEs - start of 1st experiment
  tsk   <- tskn
  tcr   <- tcrn
  skbf  <- skbfn
  mshiv <- 0
  alfa  <- .1
  rmm    <- m
  esk   <- .1 * met
  
  # UNIT CONVERSIONS (from input variables)
  atm   <- pb / 760 # input unit is torr!
  timeh <- ltime / 60 
  rcl   <- .155 * clo
  facl  <- 1 + .15 * clo  # Increase in body surface area due to clothing
  lr    <- 2.2 / atm     # Lewis Relation is 2.2 at sea level
  pa    <- rh * exp(18.6686 - (4030.183/(ta + 235))) / 100
  
  if (clo <= 0){
    wcrit <- .38 * vel ^ (-.29)
    icl   <- 1 
  } else {
    wcrit <- .59 * vel ^ (-.08)
    icl   <- .45
  }
  
  chc <- 3 * atm  ^ .53
  if(obj == "set"){ # not used for calculation of adjusted pmv
    if(rmm/58.2 <.85){
      chca<-0
    } else {
      chca<-5.66*((rmm/58.2-.85)*atm) ^ .39
    }
  }
  chcv <- 8.600001 * (vel * atm) ^ .53
  if(obj == "set"){ # not used for calculation of adjusted pmv
    if(chc<=chca){chc<-chca}
  }
  chc <- max(chc, chcv) 
  
  # initial estimate of tcl
  chr <- 4.7
  ctc <- chr + chc
  ra <- 1 / (facl * ctc) #resistance of air layer to dry heat transfer
  top <- (chr * tr + chc * ta) / ctc
  tcl <- top + (tsk - top) / (ctc * (ra + rcl))
  tim <- 1
  
  ################# BEGIN ITERATION
  
  # tcl and chr are solved iteritively using: H(tsk - to) <- ctc(tcl - to), 
  # where H <- 1/(ra + rcl) and ra <- 1/facl*ctc
  
  fnsvp  <- function(T){exp(18.6686 - 4030.183 / (T + 235))}
  fnp    <- function(x){x * (-1) * (x > 0)}
  fnfar  <- function(T){9 / 5 * T + 32}
  fnerre <- function(x, hsk, hd, tsk, w, he, pssk){hsk - (hd * (tsk - x)) - w * he.s * (pssk - .5 * fnsvp(x))}
  fnerrs <- function(x, hsk, hd.s, tsk, w, he.s, pssk){hsk - hd.s * (tsk - x) - w * he.s * (pssk - .5 * fnsvp(x))}
  
  tclold <- tcl
  flag   <- FALSE
  for (tim in 1:ltime){
    if (flag){
      tcl <- (ra * tsk + rcl * top) / (ra + rcl)
      if (abs(tcl - tclold) > .01){
        flag   <- FALSE
        tclold <- tcl
      } else {
        flag <- TRUE
      }
    }
    while (!flag){
      if(tolower(gsub(" ", "", bodyPosition, fixed = TRUE)) == 'sitting'){
        chr <- 4.0 * 0.95 * sbc * (((tcl + tr) / 2.0 + 273.15) ^ 3.0) * 0.7
      }
      else{
        chr <- 4.0 * 0.95 * sbc * (((tcl + tr) / 2.0 + 273.15) ^ 3.0) * 0.73
      }
      ctc <- chr + chc
      ra  <- 1 / (facl * ctc) # resistance of air layer to dry heat transfer
      top <- (chr * tr + chc * ta) / ctc
      tcl <- (ra * tsk + rcl * top) / (ra + rcl)
      if (abs(tcl - tclold) > .01){
        flag   <- FALSE
        tclold <- tcl
      } else {
        flag   <- TRUE
      }
    } #IF ABs(tcl-tclold)>.01 TheN tcl.OLD<-tcl: GOto 1160
    dry  <- (tsk - top) / (ra + rcl)
    hfcs <- (tcr - tsk) * (5.28 + 1.163 * skbf)
    eres <- .0023 * m * (44 - pa)
    cres <- .0014 * m * (34 - ta)
    scr  <- m - hfcs - eres - cres - w # m and w in w/2
    ssk  <- hfcs - dry - esk
    tcsk <- .97 * alfa * wt
    tccr <- .97 * (1 - alfa) * wt
    dtsk <- (ssk * sa) / tcsk / 60 # deg C per minute
    dtcr <- scr * sa / tccr / 60 # deg C per minute
    dtim <- 1 #minutes
    # U<-ABs(dtsk): IF U>.1 TheN dtim<-.1/U
    # U<-ABs(dtcr): IF U>.1 AND .1/U<dtim TheN dtim<-.1/U
    tsk <- tsk + dtsk * dtim
    tcr <- tcr + dtcr * dtim
    tb  <- alfa * tsk + (1 - alfa) * tcr
    sksig <- tsk - tskn
    warms<-fnp(sksig)
    colds<-fnp(-sksig)
    if (tsk > tskn){
      warms <- tsk - tskn
      colds <- 0
    } else {
      colds <- tskn - tsk
      warms <- 0
    }
    crsig <- (tcr - tcrn)
    warmc<-fnp(crsig)
    coldc<-fnp(-crsig)
    if (tcr > tcrn){
      warmc <- tcr - tcrn
      coldc <- 0
    } else {
      coldc <- tcrn - tcr
      warmc <- 0
    }
    bdsig<-tb-tbn
    warmb<-fnp(bdsig)
    #coldb<-fnp(-bdsig)
    if (tb > tbn){
      warmb <- tb - tbn
      coldb <- 0
    } else {
      coldb <- tbn - tb
      warmb <- 0
    }
    skbf <- (skbfn + cdil * warmc) / (1 + cstr * colds)
    if (skbf > 90){skbf <- 90}
    if (skbf < .5){skbf <- .5}
    regsw <- csw * warmb * exp(warms / 10.7)
    ### careful because it can be wrong
    if (regsw > 500){regsw <- 500}
    ersw <- .68 * regsw
    # lr <- 2.02*(tsk+273.15)/273.15
    rea  <- 1 / (lr * facl * chc) #evaporative resistance of air layer
    recl <- rcl / (lr * icl) #evaporative resistance of clothing (icl<-.45)
    emax <- (fnsvp(tsk) - pa) / (rea + recl)
    prsw <- ersw / emax### check these if the skin search for "W" 
    pwet <- .06 + .94 * prsw
    edif <- pwet * emax - ersw
    esk  <- ersw + edif
    if (pwet > wcrit){
      pwet <- wcrit
      prsw <- (wcrit) / .94 # (wcrit-.06)/.94
      ersw <- prsw * emax
      edif <- .06 * (1 - prsw) * emax
      esk  <- ersw + edif
    }
    if (emax < 0){
      edif <- 0
      ersw <- 0
      pwet <- wcrit
      prsw <- wcrit
      esk  <- emax
    }
    mshiv <- 19.4 * colds * coldc
    m     <- rmm + mshiv
    alfa  <- .0417737 + .7451833 / (skbf + .585417)
    #GOsUB2680 'screen output
    #IF OUtopT<-1 TheN GOsUB 3900 'minute-by-minute hcopy output
    #tim <- tim+dtim #IF tim<=ltime TheN GOto 1200
  }
  
  #####################################################################
  # CALCULATE COMFORT INDICES
  #####################################################################
  
  # Define new heat flow terms, coeffs, and abbreviations
  store   <- m - w - cres - eres - dry - esk     #rate of body heat storage #?
  hsk     <- dry + esk                   #total heat loss from skin
  rn      <- m - w                       #net metabolic heat production [w/m2]
  ecomf   <- .42 * (rn - 58.2)
  
  if (ecomf < 0){ecomf <- 0} #from Fanger
  
  ereq    <- rn - eres - cres - dry#?
  emax    <- emax * wcrit
  hd      <- 1 / (ra + rcl) #?
  he      <- 1 / (rea + recl)#?
  wet     <- pwet
  pssk    <- fnsvp(tsk)
  
  #Definition of ASHRAE standard environment... denoted "s"
  chrs <- chr
  if (met < .85){
    chcs <- 3
  } else { # GOto 1950
    chcs <- 5.66 * ((met - .85)) ^ .39
    if (chcs < 3){chcs <- 3}
  }
  ctcs  <- chcs + chrs
  rclos <- 1.52 / ((met - wme) + .6944) -.1835 # here wme also in [met]!
  rcls  <- .155 * rclos
  facls <- 1 + kclo * rclos
  fcls  <- 1 / (1 + .155 * facls * ctcs * rclos)
  ims   <- .45
  icls  <- (ims * chcs / ctcs * (1 - fcls)) / (chcs / ctcs - fcls * ims)
  ras   <- 1 / (facls * ctcs)
  reas  <- 1 / (lr * facls * chcs)
  recls <- rcls / (lr * icls)
  hd.s  <- 1 / (ras + rcls)
  he.s  <- 1 / (reas + recls)
  
  # et* (standardized humidity/ actual do, pb, and chc)
  # determined using Newton's iterative solution
  # fnerre is defined in general setup section above
  delta <- .0001
  xold <- tsk - hsk / hd.s #lower bound for et*
  
  flag1 <- FALSE
  while (!flag1){
    err1 <- fnerre(xold, hsk, hd, tsk, wet, he, pssk)
    err2 <- fnerre(xold + delta, hsk, hd, tsk, wet, he, pssk)
    x <- xold - delta * err1 / (err2 - err1)
    if (abs(x - xold) > .01){
      xold <- x
      flag1 <- FALSE
    } else {
      flag1 <- TRUE
    }
  } #IF ABs (x-xold)>.01 TheN xold<-x: GOto 2120
  et <- x
  
  # set* (standardized humidity, clo, pb, and chc)
  # determined using Newton's iterative solution
  # fnerrs is defined in the GENEraL setuP section above
  
  xold <- tsk-hsk / hd.s #lower bound for set
  flag2 <- FALSE
  
  while (!flag2){
    err1 <- fnerrs(xold, hsk, hd.s, tsk, wet, he.s, pssk)
    err2 <- fnerrs(xold + delta, hsk, hd.s, tsk, wet, he.s, pssk)
    
    x <- xold - delta * err1 / (err2 - err1)
    
    if (abs(x - xold) > .01){
      xold <- x
      flag2 <- FALSE
    } else {
      flag2 <- TRUE
    }
  } # IF ABs(x-xold)>.01 TheN xold<-x: GOto 2220
  set <- x
  
  # sto <- standard operative temperature:
  # defined by: (tsk-sto)/(ras+rcls)<-(tsk-to)/(ra + rcl)
  sto <- tsk - (ras + rcls) * (tsk - top) / (ra + rcl)
  
  # sVPO <- standard operative vapor pressure:
  # defined by: (psk - sVPO)/(reas+recls) <- (psk-pa)/(rea+recl)
  # sVPO <- psK - (reas+recls)*(psK-pa)/(rea+recl)
  
  # tsens is a function of tb 
  tbml <- (.194 / 58.15) * rn + 36.301 #lower limit for evaporative regulation
  tbmh <- (.347 / 58.15) * rn + 36.669 #upper limit for evaporative regulation
  if (tb < tbml){
    tsens <- .4685 * (tb - tbml)
  } else if (tb >= tbml & tb < tbmh){
    tsens <- wcrit * 4.7 * (tb - tbml) / (tbmh - tbml)
  } else if (tb >= tbmh){
    tsens <- wcrit * 4.7 + .4685 * (tb - tbmh)
  }
  
  # disc varies with relative thermoregulatory heat strain
  # Valid only when disc>0. when disc<0, disc is numerically <-tsens.
  disc <- 4.7 * (ersw - ecomf) / (emax - ecomf - edif)
  if (disc < 0){disc <- tsens}
  
  # Calculate Gagge's version of Fanger's Predicted Mean Vote (PMV)
  pmvg <- (.303 * exp(-.036 * m) + .028) * (ereq - ecomf - edif)
  
  #Gagge's pmv.set is the same as Fanger's pmv except that dry is calculated
  #using set* rather than top
  dry2  <- hd.s * (tsk - set)
  ereq2 <- rn - cres - eres - dry2
  pmvstar  <- (.303 * exp(-.036 * m) + .028) * (ereq2 - ecomf - edif)
  
  # other indices related to air movement
  tue  <- (34 - ta) * (vel - 0.05) ^ 0.6223
  pd   <- tue * (3.143 + 0.3696 * vel * tu)
  ps   <- 100 * (1.13 * (top ^ .5) - .24 * top + 2.7 * (vel ^ .5) - .99 * vel)
  pts  <- .25 * set - 6.03
  
  if(varOut=="skinWet"){
    output <- data.frame(wet)
  } else {
    output <- data.frame(et, set, tsens, disc, pd, ps, pts, pmvg, pmvstar, tsk, tcr, wet, esk, regsw)
  }
  rm(et, set, tsens, disc, pd, ps, pts)
  output
}
