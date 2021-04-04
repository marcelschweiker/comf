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
  return(0.61078*10^(7.5*x/(x+237.3)))
}
##############################################

BSAst <- function(){
  c(0.110, 0.029, 0.175, 0.161, 0.221,0.096, 0.063, 0.050, 0.096, 0.063, 0.050,
    0.209, 0.112, 0.056, 0.209, 0.112, 0.056)
}

##############################################

calcWeightrate <- function(weight=74.43){
  
  rate <- weight / 74.43
  return(rate)
}

##############################################

calcBMR <- function(height = 1.72, weight = 74.43, age = 20, 
                    sex="male", equation="harris-benedict"){
  BMR = switch(
    equation,
    "harris-benedict" = {
      if(sex=="male") 88.362 + 13.397*weight + 500.3*height - 5.677*age 
      else 447.593 + 9.247*weight + 479.9*height - 4.330*age
    },
    "harris-benedict_origin" = {
      if(sex=="male") 66.4730 + 13.7516*weight + 500.33*height - 6.7550*age 
      else 655.0955 + 9.5634*weight + 184.96*height - 4.6756*age},
    {
      if(sex=="male") (0.0481*weight + 2.34*height - 0.0138*age - 0.4235) * 238.8915
      else (0.0481*weight + 2.34*height - 0.0138*age - 0.9708) * 238.8915}
  )
  BMR * 0.048
}

##############################################

calcPreferredTemp <- function(va=0.1, rh =50, met=1, clo=0){
  to <- 28
  for(i  in seq(1,100)){
    vpmv <- calcPMV(to, to, va, rh, met, clo)
    if(abs(vpmv) < 0.001) break
    else to <- to - vpmv/3
  }
  to
}

##############################################

options <- function(){
  list("nonshivering_thermogenesis"= TRUE, "cold_acclimated"= FALSE,
       "shivering_threshold"= FALSE, "limit_dshiv/dt"= FALSE,
       "bat_positive"= FALSE, "ava_zero"= FALSE, "shivering"= FALSE)
}

##############################################

run <- function(dtime=60, passive=False, output=True, exOutput = NULL, 
                modelName = "JOS3"){
  tcr <- Tcr()
  tsk <- Tsk()
  
  # Convective and radiative heat transfer coefficient [W/K.m2]
  posture <- "standing"
  va <- rep(0.1,17)
  ta <- rep(28.8,17)
  hc <- calcfixedHC(convCoef(posture, va, ta, tsk), va)
  hr <- calcfixedHR(radCoef(posture))
  
  # Operarive temp. [oC], heat and evaporative heat resistance [K/W], [kPa/W]
  tr <- rep(28.8,17)
  clo <- rep(0, 17)
  iclo <- rep(0.45, 17)
  to <- calcoperativeTEMP(ta, tr, hc, hr)
  rT <- calcdryR(hc, hr, clo)
  rEt <- calcwetR(hc, clo, iclo) 
  
  # Thermoregulation
  
  # Setpoint temperature of thermoregulation
  if(passive){
    setptCr <- tcr
    setptSk <- tsk
  }
  else{
    setptCr <- rep(37, 17)
    setptSk <- rep(34, 17)
  }
  
  # Difference between setpoint and body temperatures
  errCr <- tcr - setptCr
  errSk <- tsk - setptSk
  
  # Skinwettedness [-], Esk, Emax, Esw [W]
  rh <- rep(50, 17)
  height <- 1.72
  weight <- 74.43
  bsaEquation <- "dubois"
  age <- 20
  wet <- evaporation(errCr, errSk, tsk, ta, rh, rEt, height, weight, bsaEquation,
                     age)
  eSk <- wet
  eMax <- wet
  eSweat <- wet
  
  # Skin blood flow, basal skin blood flow [L/h]
  ci <- 2.59
  bfSk <- skinBloodflow(errCr, errSk, height, weight, bsaEquation, age, ci)
  
  # Hand, Foot AVA blood flow [L/h]
  options <- options()
  bfAvaHand <- avaBloodflow(errCr, errSk, height, weight, bsaEquation, age, ci)
  bfAvaFoot <- bfAvaHand
  if(options[["ava_zero"]] & passive){
    bfAvaHand <- 0
    bfAvaFoot <- 0
  }
  
  # Thermogenesis by shivering [W]
  sex <- "male"
  mshiv <- shivering(errCr, errSk, tcr, tsk, height, weight, bsaEquation, age, 
                     sex, dtime, options)
  
  # Thermogenesis by non-shivering [W]
  if(options[["nonshivering_thermogenesis"]]){
    mnst <- nonshivering(errCr, errSk, height, weight, bsaEquation, age, 
                        options[["cold_acclimated"]], options[["bat_positive"]])
  }
  else{
    mnst <- rep(0, 17)
  }
  
  # Thermogenesis
  # Basal thermogenesis [W]
  mbase <- localMbase(height, weight, age, sex, bsaEquation)
  mbaseAll <- 0
  for(m in mbase){
    mbaseAll <- mbaseAll + sum(m) 
  }
  
  # Thermogenesis by work [W]
  par <- 1.25
  mwork <- localMwork(mbaseAll, par)
  
  # Sum of thermogenesis in core, muscle, fat, skin [W]
  sumMRes <- sumM(mbase, mwork, mshiv, mnst)
  qcr <- sumMRes$qcr
  qms <- sumMRes$qms
  qfat <- sumMRes$qfat
  qsk <- sumMRes$qsk
  
  qall <-  sum(qcr) + sum(qms) + sum(qfat) + sum(qsk)
  
  # Other
  # Blood flow in core, muscle, fat [L/h]
  crmsfatBloodflowRes <- crmsfatBloodflow(mwork, mshiv, height, weight, 
                                          bsaEquation, age, ci)
  bfCr <- crmsfatBloodflowRes$bfCr
  bfMs <- crmsfatBloodflowRes$bfMs
  bfFat <- crmsfatBloodflowRes$bfFat
  
  # Heat loss by respiratory
  pA <- rep(NA, 17)
  antoineRes <- antoine(ta)
  for(i in 1:length(pa)){
    pA[i] <- antoine[i] * rh[i] /100
  }
  respHeatlossRes <- respHeatloss(ta[0], pA[0], qall)
  resSh <- respHeatlossRes[1]
  resLh <- respHeatlossRes[2]
  
  # Sensible heat loss [W]
  bsa <- localBsa(height, weight, bsaEquation)
  shlsk <- (tsk - to) / rT * bsa
  
  # Cardiac output [L/h]
  co <- sumBf(bfCr, bfMs, bfFat, bfSk, bfAvaHand, bfAvaFoot)
  
  # Weight loss rate by evaporation [g/sec]
  wlesk = (eSweat + 0.06*eMax) / 2418
  wleres <- resLh / 2418
  
  #------------------------------------------------------------------
  # Matrix
  #------------------------------------------------------------------
  # Matrix A
  # (83, 83,) ndarray
  vesselBloodflowRes <- vesselBloodflow(bfCr, bfMs, bfFat, bfSk, bfAvaHand, 
                                         bfAvaFoot)
  bfArt <- vesselBloodflowRes[1]
  bfVein <- vesselBloodflowRes[2]
  
  bfLocal <- localArr(bfCr, bfMs, bfFat, bfSk, bfAvaHand, bfAvaFoot)
  bfWhole <- wholebody(bfArt, bfVein, bfAvaHand, bfAvaFoot)
  numNodes <- calcIndexOrder()[[2]]
  arrBF <- matrix(0, nrow <- numNodes, ncol <- numNodes)
  for(i in 1:length()){
    pA[i] <- antoine[i] * rh[i] /100
  }
  cap <- capacity(height, weight, bsaEquation, age, ci)
  
  for (i in 1:nrow(arrBF)) {
    for (j in 1:ncol(arrBF)) {
      arrBF[i,j] <- arrBF[i,j] + bfLocal[i,j] + bfWhole[i,j]
      arrBF[i,j] <- arrBF[i,j]/cap[i] # Change unit [W/K] to [/sec]
      arrBF[i,j] <- arrBF[i,j]*dtime # Change unit [/sec] to [-]
    }
  }
  
  cdt <- conductance(height, weight, bsaEquation, fat)
  arrCdt <- cdt
  for (i in 1:nrow(arrCdt)) {
    for (j in 1:ncol(arrCdt)) {
      arrCdt[i,j] <- arrCdt[i,j]/cdt[i] # # Change unit [W/K] to [/sec]
      arrCdt[i,j] <- arrCdt[i,j]*dtime # Change unit [/sec] to [-]
    }
  }
  
  arrB <- rep(0, numNodes)
  index <- calcIndex()
  arrB[index["skin"]] = arrB[index["skin"]] + (1/rT*bsa)
  
  for (i in 1:length(arrB)) {
    arrB[i] = arrB[i] / cap[i] # Change unit [W/K] to [/sec]
    arrB[i] = arrB[i] * dtime # Change unit [/sec] to [-]
  }
  
  arrATria <- matrix(0, nrow <- numNodes, ncol <- numNodes)
  for (i in 1:nrow(arrATria)) {
    for (j in 1:ncol(arrATria)) {
      arrATria[i,j] <- arrATria[i,j] - (arrCdt[i,j] + arrBF[i,j])
    }
  }
  
  arrADia <- matrix(0, nrow <- numNodes, ncol <- numNodes)
  for (i in 1:nrow(arrADia)) {
    for (j in 1:ncol(arrADia)) {
      arrADia[i,j] <- arrADia[i,j] + (arrCdt[i,j] + arrBF[i,j])
    }
  }
  for (i in 1:nrow(arrADia)) {
    for (j in 1:ncol(arrADia)) {
      arrADia[i,j] <- arrADia[i,j] + sum(arrADiai[i,]) + arrB[i]
    }
  }
  diagArray <- diag(numNodes)
  for (i in 1:nrow(arrADia)) {
    for (j in 1:ncol(arrADia)) {
      arrADia[i,j] <- diag(arrADia, names <- TRUE)
      arrADia[i,j] <-  arrADia[i,j] + diagArray[i,j]
    }
  }
  
  arrA <- matrix(0, nrow <- numNodes, ncol <- numNodes)
  for (i in 1:nrow(arrA)) {
    for (j in 1:ncol(arrA)) {
      arrA[i,j] <- arrA[i,j] + arrATria[i,j] + arrADia[i,j]
    }
  }
  
  arraAInv <- solve(arrA)
  
  # Matrix Q [W] / [J/K] * [sec] = [-]
  # Thermogensis
  vIndex <- calcVIndex()
  arrQ <- rep(0, numNodes)
  arrQ[index["core"]] <- arrQ[index["core"]] + qcr
  arrQ[index["muscle"]] <- arrQ[index["muscle"]] + qms[vIndex["muscle"]]
  arrQ[index["fat"]] <-  arrQ[index["fat"]] + qfat[vIndex["fat"]]
  arrQ[index["skin"]] <- arrQ[index["skin"]] + qsk
  
  # Respiratory [W]
  arrQ[index["core"][3]] <- arrQ[index["core"][3]] - (resSh + resLh) #Chest core
  
  # Sweating [W]
  arrQ[index["skin"]] <- arrQ[index["skin"]] - eSk
  
  # Extra heat gain [W]
  exQ <- rep(0, numNodes)
  for (i in 1:length(arrQ)) {
    arrQ[i] <- arrQ[i] + exQ[i]
    arrQ[i] <- arrQ[i]/cap[i] # Change unit [W]/[J/K] to [K/sec]
    arrQ[i] <- arrQ[i] * dtime # Change unit [K/sec] to [K]
  }
  
  # Boundary batrix [℃]
  arrTo <- rep(0, numNodes)
  arrTo[index["skin"]] <- arrTo[index["skin"]] + to
  
  # all
  bodyTemp <- calcBodyTemp()
  arr <- rep(0, length(bodyTemp))
  for (i in length(arr)) {
    arr[i] <- arr[i] + bodyTemp[i] + arrB[i] * arrTo[i] + arrQ[i]
  }
  #------------------------------------------------------------------
  # New body temp. [oC]
  #------------------------------------------------------------------
  bodyTemp <- (arraAInv %*% t(arr))
  
  #------------------------------------------------------------------
  # Output paramters
  #------------------------------------------------------------------
  dictout <- list()
  if(output){
    cycle <- 0
    dictout["CycleTime"] <- cycle
    t <- 0
    dictout["ModTime"] <- t
    dictout["dt"] <- dtime
    dictout["TskMean"] <- calcTskMean()
    dictout["Tsk"] <- tsk
    dictout["Tcr"] <- tcr
    sumWet <- 0
    for (i in 1:length(wet)) {
      sumWet <- sumWet + (wet[i] * BSAst()[i])
    }
    dictout["WetMean"] <- sumWet/length(wet)
    dictout["Wet"] <- wet
    dictout["Wle"] <- sum(wlesk) + wleres
    dictout["CO"] <- co
    dictout["Met"] <- qall
    dictout["RES"] <- resSh + resLh
    dictout["THLsk"] <- shlsk + eSk
  }
  
  detailout <- list()
  if(exOutput){
    detailout["Name"] <- model_name
    detailout["Height"] <- height
    detailout["Weight"] <- weight
    detailout["BSA"] <- bsa
    detailout["Fat"] <- fat
    detailout["Sex"] <- sex
    detailout["Age"] <- age
    detailout["Setptcr"] <- setptCr
    detailout["Setptcr"] <- setptSk
    detailout["Tcb"] <- Tcb()
    detailout["Tar"] <- Tar()
    detailout["Tve"] <- Tve()
    detailout["Tsve"] <- Tsve()
    detailout["Tms"] <- Tms()
    detailout["Tfat"] <- Tfat()
    detailout["To"] <- to
    detailout["Rt"] <- rT
    detailout["Ret"] <- rEt
    detailout["Ta"] <- ta
    detailout["Tr"] <- tr
    detailout["RH"] <- rh
    detailout["Va"] <- va
    detailout["PAR"] <- par
    detailout["Icl"] <- clo
    detailout["Esk"] <- eSk
    detailout["Emax"] <- eMax
    detailout["Esweat"] <- eSweat
    detailout["BFcr"] <- bfCr
    detailout["BFms"] <- bfMs[vIndex["muscle"]]
    detailout["BFfat"] <- bfFat[vIndex["fat"]]
    detailout["BFsk"] <- bfSk
    detailout["BFava_hand"] <- bfAvaHand
    detailout["BFava_foot"] <- bfAvaFoot
    detailout["Mbasecr"] <- mbase[1]
    detailout["Mbasems"] <- mbase[2][vIndex["muscle"]]
    detailout["Mbasefat"] <- mbase[3][vIndex["fat"]]
    detailout["Mbasesk"] <- mbase[4]
    detailout["Mwork"] <- mwork
    detailout["Mshiv"] <- mshiv
    detailout["Mnst"] <- mnst
    detailout["Qcr"] <- qcr
    detailout["Qms"] <- qms[vIndex["muscle"]]
    detailout["Qfat"] <- qfat[vIndex["fat"]]
    detailout["Qsk"] <- qsk
    dictout["SHLsk"] <- shlsk
    dictout["LHLsk"] <- eSk
    dictout["RESsh"] <- resSh
    dictout["RESlh"] <- resLh
  }
  
  if(exOutput == "all"){
    dictout <- c(dictout, detailout)
  } else if(typeof(ex_output) == "list"){  # if ex_out type is list
    outkeys <- names(detailout)
    for (i in exOutput) {
      if(i %in% outkeys){
        dictout[key] <- detailout[key]
      }
    }
  }
  return(dictout)
}

##############################################

bodyNames <- function(){
  c("Head", "Neck", "Chest", "Back", "Pelvis", "LShoulder", "LArm", "LHand",
    "RShoulder", "RArm", "RHand","LThigh", "LLeg", "LFoot", "RThigh", "RLeg", 
    "RFoot")
}

##############################################

layerNames <- function(){
  c("artery", "vein", "sfvein", "core", "muscle", "fat", "skin")
}

##############################################

calcIndexOrder <- function(){
  indexDict <- list()
  
  for(key in c("Head", "Pelvis")){
    tempDict <- list("artery"= 1, "vein"= 1, "sfvein"= NA, "core"= 1, "muscle"= 1, 
                    "fat"= 1, "skin"= 1)
    indexDict[[key]]= tempDict
  }
  
  for (key in c("Neck", "Chest", "Back")) {
    tempDict <- list("artery"= 1, "vein"= 1, "sfvein"= NA, "core"= 1, 
                    "muscle"= NA, "fat"= NA, "skin"= 1)
    indexDict[[key]]= tempDict
  }
  
  for(key in bodyNames()[-(1:5)]){
    tempDict <- list("artery"= 1, "vein"= 1, "sfvein"= 1, "core"= 1, 
                    "muscle"= NA, "fat"= NA, "skin"= 1)
    indexDict[[key]]= tempDict
  }
  indexDict["CB"] = 0
  orderCount <- 1
  for(bn in bodyNames()){
    for(ln in layerNames()){
      if(!is.na(indexDict[[bn]][[ln]])){
        indexDict[[bn]][[ln]] = orderCount
        orderCount <- orderCount + 1
      }
    }
  }
  list(indexDict, orderCount)
}

##############################################

calcBodyTemp <- function(){
  numNodes <- calcIndexOrder()[[2]]
  bodyTemp <- rep.int(36, numNodes)
  bodyTemp
}

##############################################

calcIndexByLayer <- function(layer){
  outIndex <- c()
  for(bn in bodyNames()){
    for(ln in layerNames()){
      
    }
  }
    
}

##############################################

Tcr <- function(){
  10
}

##############################################

Tsk <- function(){
  10
}

##############################################

convCoef <- function(hc, va) {
  
}

##############################################

radCoef <- function(posture){
  
}

##############################################

options <- function(){
  list("nonshivering_thermogenesis" = TRUE, 
       "cold_acclimated" = FALSE, 
       "shivering_threshold" = FALSE, 
       "limit_dshiv/dt" = FALSE, 
       "bat_positive" = FALSE, 
       "ava_zero" = FALSE,
       "shivering" = FALSE)
}

##############################################

localMbase <- function(){
  return(mapply(c, c(1,2), c(2,3), SIMPLIFY<-FALSE))
}

##############################################

sumM <- function(mbase, mwork, mshiv, mnst){
 list(qcr <- c(1,2), qms <- c(1,2), qfat <- c(1,2), qsk <- c(1,2)) 
}

##############################################

crmsfatBloodflow <- function(mwork, mshiv, height=1.72, weight=74.43, 
                             equation="dubois", age=20, ci=2.59){
  list(bfCr <- c(1,2), bfMs <- c(1,2), bfFat <- c(1,2)) 
}

##############################################

respHeatloss <- function(t, p, met){
  list(1,2)
}

##############################################

localBsa <- function(height=1.72, weight=74.43, equation="dubois"){
  return(1)
}

##############################################

sumBf <- function(bfCr, bfMs, bfFat, bfSk, bfAvaHand, bfAvaFoot){
  return(1)
}

##############################################

vesselBloodflow <- function(bfCr, bfMs, bfFat, bfSk, bfAvaHand, bfAvaFoot){
  return(NULL)
}

##############################################

localArr <- function(bfCr, bfMs, bfFat, bfSk, bfAvaHand, bfAvaFoot){
  return(NULL)
}

##############################################

wholebody <- function(bfArt, bfVein, bfAvaHand, bfAvaFoot){
  return(NULL)
}

##############################################

capacity <- function(height, weight, bsaEquation, age, ci){
  return(rep(1, 85))
}

##############################################

indexBylayer <- function(layer){
  
}

##############################################

calcIndex <- function(){
}

###############################################

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
