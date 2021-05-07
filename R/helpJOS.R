#######################################################
#run all these functions before running JOS3-functions
#######################################################

# Initial body temp [oC]
indexOrderRes <- calcIndexOrder()
idict <- indexOrderRes[[1]]
numNodes <- indexOrderRes[[2]]
bodytemp <- rep(36, numNodes)

layerNames <- c("artery", "vein", "sfvein", "core", "muscle", "fat", "skin")
bodyNames <-c("Head", "Neck", "Chest", "Back", "Pelvis", "LShoulder", "LArm", 
              "LHand", "RShoulder", "RArm", "RHand","LThigh", "LLeg", "LFoot", 
              "RThigh", "RLeg", "RFoot")


# Constant parameters of the matrix' indicies
indexValuesRes <- calcIndexValues()
index <- indexValuesRes[[1]]
vIndex <- indexValuesRes[[2]]

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

###############################################

BSAst <- c(0.110, 0.029, 0.175, 0.161, 0.221,0.096, 0.063, 0.050, 0.096, 0.063, 
           0.050, 0.209, 0.112, 0.056, 0.209, 0.112, 0.056)

###############################################

calcWeightrate <- function(weight=74.43){

  rate <- weight / 74.43
  return(rate)
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

run <- function(dtime=60, passive=FALSE, output=True, posture = "standing", va, 
                ta, options, tr, clo, iclo, height, bsaEquation, weight, age, ci,
                sex, par){
  #  Run a model for a once and get model parameters.
  
  tcr <- Tcr()
  tsk <- Tsk()
  
  # Convective and radiative heat transfer coefficient [W/K.m2]
  #hc <- calcfixedHC(calcConvCoef(posture, va, ta, tsk), va)
  #hr <- calcfixedHR(calcRadCoef(posture))
  hc <- c(4.47993,4.47993,2.96995,2.90995,2.84995,3.60994,3.54994,3.66994,3.60994,3.54994,3.66994,2.79996,2.03997,2.03997,2.79996,2.03997,2.03997)
  hr <- c(4.89011,4.89011,4.32010,4.09009,4.32010,4.55010,4.43010,4.21010,4.55010,4.43010,4.21010,4.77011,5.34012,6.14014,4.77011,5.34012,6.14014)

  # Operarive temp. [oC], heat and evaporative heat resistance [K/W], [kPa/W]
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
  evaporationMetrics <- calcEvaporation(errCr, errSk, tsk, ta, rh, rEt, height, 
                                        weight, bsaEquation, age)
  wet <- evaporationMetrics$wet
  eSk <- evaporationMetrics$e_sk
  eMax <- evaporationMetrics$e_max
  eSweat <- evaporationMetrics$e_sweat
  
  # Skin blood flow, basal skin blood flow [L/h]
  bfSk <- calcSKINbloodflow(errCr, errSk, height, weight, bsaEquation, age, ci)
  # Hand, Foot AVA blood flow [L/h]
  avaBloodflow <- calcAVAbloodflow(errCr, errSk, height, weight, bsaEquation, 
                                   age, ci)
  bfAvaHand <- avaBloodflow$blooflowhand
  bfAvaFoot <- avaBloodflow$blooflowfoot

  if(options[["ava_zero"]] & passive){
    bfAvaHand <- 0
    bfAvaFoot <- 0
  }
  
  # Thermogenesis by shivering [W]
  mshiv <- calcShivering(errCr, errSk, tcr, tsk, height, weight, bsaEquation, age, 
                     sex, dtime, options)
  
  # Thermogenesis by non-shivering [W]
  if(options[["nonshivering_thermogenesis"]]){
    mnst <- calcNOshivering(errCr, errSk, height, weight, bsaEquation, age, 
                        options[["cold_acclimated"]], options[["bat_positive"]])
  }
  else{
    mnst <- rep(0, 17)
  }
  
  # Thermogenesis
  # Basal thermogenesis [W]
  mbase <- calcMbaselocal(height, weight, age, sex, bsaEquation)
  mbaseAll <- 0
  for(m in mbase){
    mbaseAll <- mbaseAll + sum(m) 
  }
  #this will be removed
  mbaseAll <- 87.96
  
  # Thermogenesis by work [W]
  mwork <- calcMworklocal(mbaseAll, par)

  # Sum of thermogenesis in core, muscle, fat, skin [W]
  sumM <- calcSUMm(mbase, mwork, mshiv, mnst)
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
  antoineRes <- calcAntoine(ta)
  for(i in 1:length(pA)){
    pA[i] <- antoineRes[i] * rh[i] /100
  }
  respHeatlossRes <- respHeatloss(ta[0], pA[0], qall)
  resSh <- respHeatlossRes[1]
  resLh <- respHeatlossRes[2]
  
  # Sensible heat loss [W]
  bsa <- calcBSAlocal(height, weight, bsaEquation)
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
  
  cdt <- calcConductance(height, weight, bsaEquation, fat)
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
  
  for(key in bodyNames[-(1:5)]){
    tempDict <- list("artery"= 1, "vein"= 1, "sfvein"= 1, "core"= 1, 
                    "muscle"= NA, "fat"= NA, "skin"= 1)
    indexDict[[key]]= tempDict
  }
  indexDict["CB"] = 0
  orderCount <- 1
  for(bn in bodyNames){
    for(ln in layerNames){
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
  for(bn in bodyNames){
    for(ln in layerNames){
      if((tolower(layer) == ln) & (!is.null(idict[[bn]][[ln]]))){
        outIndex <- append(outIndex, idict[[bn]][[ln]])
      }
    }
  }
  outIndex
}

##############################################

calcValidIndexByLayer <- function(layer){
  # Get indices of the matrix by the layer name
  outIndex <- c()
  for(i in 1:length(bodyNames)){
    layerValue <- idict[[bodyNames[i]]][[layer]]
    if(!is.null(layerValue)){
      outIndex <- append(outIndex, i)
    }
  }
  outIndex
}

##############################################

calcIndexValues <- function(){
  index <- list()
  vIndex <- list()
  for(key in layerNames){
    index[[key]] <- calcIndexByLayer(key)
    vIndex[[key]] <- calcValidIndexByLayer(key)
  }
  return(list(index, vIndex))
}

##############################################

Tcr <- function(){
  bodytemp[index[["core"]]]
}

##############################################

Tsk <- function(){
  bodytemp[index[["skin"]]]
}

##############################################

calcConvCoef <- function(posture="standing", va=0.1, ta=28.8, tsk=34.0) {
  # Calculate convective heat transfer coefficient (hc) [W/K.m2]
  hcA <- NULL
  hcB <- NULL
  hcNatural <- NULL
  
  # Natural convection
  if(tolower(posture) == 'standing'){
    # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
    hcNatural <- c(4.48, 4.48, 2.97, 2.91, 2.85, 3.61, 3.55, 3.67, 3.61, 3.55, 
                   3.67, 2.80, 2.04, 2.04, 2.80, 2.04, 2.04)
  }
  else if(tolower(posture) %in% c("sitting", "sedentary")){
    # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
    hcNatural <- c(4.75, 4.75, 3.12, 2.48, 1.84, 3.76, 3.62, 2.06, 3.76, 3.62, 
                   2.06, 2.98, 2.98, 2.62, 2.98, 2.98, 2.62)
  }
  else if(tolower(posture) %in% c("lying", "supine")){
    # Kurazumi et al., 2008, https://doi.org/10.20718/jjpa.13.1_17
    # The values are applied under cold environment.
    hcA <- c(1.105, 1.105, 1.211, 1.211, 1.211, 0.913, 2.081, 2.178, 0.913, 
             2.081, 2.178, 0.945, 0.385, 0.200, 0.945, 0.385, 0.200)
    hcB <- c(0.345, 0.345, 0.046, 0.046, 0.046, 0.373, 0.850, 0.297, 0.373, 
             0.850, 0.297, 0.447, 0.580, 0.966, 0.447, 0.580, 0.966)
    hcNatural <- hcA * (abs(ta - tsk) ^ hcB)
  }
  
  # Forced convection
  # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
  hcA <- c(15.0, 15.0, 11.0, 17.0, 13.0, 17.0, 17.0, 20.0, 17.0, 17.0, 20.0,
          14.0, 15.8, 15.1, 14.0, 15.8, 15.1)
  hcB <- c(0.62, 0.62, 0.67, 0.49, 0.60, 0.59, 0.61, 0.60, 0.59, 0.61, 0.60,
          0.61, 0.74, 0.62, 0.61, 0.74, 0.62)
  hcForced <- hcA * (va ^ hcB)
  
  # Select natural or forced hc.
  # If local va is under 0.2 m/s, the hc valuse is natural.
  ifelse(va<0.2, hc <- hcNatural, hc <- hcForced)
  hc
}

##############################################

calcRadCoef <- function(posture = "standing"){
  # Calculate radiative heat transfer coefficient (hr) [W/K.m2]
  
  if(tolower(posture) == "standing"){
    # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
    hr <- c(4.89, 4.89, 4.32, 4.09, 4.32, 4.55, 4.43, 4.21, 4.55, 4.43, 4.21,
      4.77, 5.34, 6.14, 4.77, 5.34, 6.14)
  }
  else if(tolower(posture) %in% c("sitting", "sedentary")){
    # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
    hr <- c(4.96, 4.96, 3.99, 4.64, 4.21, 4.96, 4.21, 4.74, 4.96, 4.21, 4.74,
           4.10, 4.74, 6.36, 4.10, 4.74, 6.36)
  }
  else if(tolower(posture) %in% c("lying", "supine")){
    # Kurazumi et al., 2008, https://doi.org/10.20718/jjpa.13.1_17
    hr <- c(5.475, 5.475, 3.463, 3.463, 3.463, 4.249, 4.835, 4.119, 4.249, 4.835, 
            4.119, 4.440, 5.547, 6.085, 4.440, 5.547, 6.085)
  }
  hr
}

##############################################

localMbase <- function(){
  return(mapply(c, c(1,2), c(2,3), SIMPLIFY<-FALSE))
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

# Calculate the thermal capacity [J/K]
calcCapacity <- function(height = 1.72, weight = 74.43, equation = "dubois", 
                         age = 20, ci = 2.59){
  
  # artery [Wh/K]
  capArt <- c(0.096, 0.025, 0.12, 0.111, 0.265, 0.0186, 0.0091, 0.0044, 0.0186, 
              0.0091, 0.0044, 0.0813, 0.04, 0.0103, 0.0813, 0.04, 0.0103)
  
  # vein [Wh/K]
  capVein <- c(0.321, 0.085, 0.424, 0.39, 0.832, 0.046, 0.024, 0.01, 0.046, 0.024, 
              0.01, 0.207, 0.1, 0.024, 0.207, 0.1, 0.024)
  
  # superficial vein [Wh/K]
  capSfv <- c(0, 0, 0, 0, 0, 0.025, 0.015, 0.011, 0.025, 0.015, 0.011, 0.074, 
             0.05, 0.021, 0.074, 0.05, 0.021)
  
  # central blood [Wh/K]
  capCb <- 1.999
  
  # core [Wh/K]
  capCr <- c(1.7229, 0.564, 10.2975, 9.3935, 13.834, 1.6994, 1.1209, 0.1536, 
            1.6994, 1.1209, 0.1536, 5.3117, 2.867, 0.2097, 5.3117, 2.867, 0.2097)
  
  # muscle [Wh/K]
  capMs <- c(0.305, 0.0, 0.0, 0.0, 7.409, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  
  # fat [Wh/K]
  capFat <- c(0.203, 0.0, 0.0, 0.0, 1.947, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
             0.0, 0.0, 0.0, 0.0)
  
  # skin [Wh/K]
  capSk <- c(0.1885, 0.058, 0.441, 0.406, 0.556, 0.126, 0.084, 0.088, 0.126, 
            0.084, 0.088, 0.334, 0.169, 0.107, 0.334, 0.169, 0.107)
  
  # Changes the values based on the standard body
  bfbr <- calcBFBrate(height, weight, equation, age, ci)
  wr <- calcWeightrate(weight)
  
  capArt <- capArt * bfbr
  capVein <- capVein * bfbr
  capSfv <- capSfv * bfbr
  capCb <-  capCb * bfbr
  capCr <- capCr * wr
  capMs <- capMs * wr
  capFat <- capFat *wr
  capSk <- capSk * wr
  
  capWhole <- rep(0, numNodes)
  capWhole[1] <- capCb
  
  for(i in 1:length(bodyNames)){
    # Dictionary of indecies in each body segment
    # key = layer name, value = index of matrix
    indexof <- idict[[bodyNames[i]]]
    
    # Common
    capWhole[indexof[["artery"]] + 1] <- capArt[i]
    capWhole[indexof[["vein"]] + 1] <- capVein[i]
    capWhole[indexof[["core"]] + 1] <- capCr[i]
    capWhole[indexof[["skin"]] + 1] <- capSk[i]
    
    # Only limbs
    if(i>5){
      capWhole[indexof[["sfvein"]] + 1] <- capSfv[i]
    }
    
    # If the segment has a muscle or fat layer
    if(!is.null(indexof[["muscle"]])){
      capWhole[indexof[["muscle"]] +1] <- capMs[i]
      capWhole[indexof[["fat"]] + 1] <- capFat[i]
    }
  }

  capWhole <- capWhole * 3600  # Changes unit [Wh/K] to [J/K]
  capWhole
}

##############################################

calcIndex <- function(){
}

###############################################

calcfixedHC <- function(hc, va){
mean_hc <- weighted.mean(hc, weights=BSAst)
mean_va <- weighted.mean(va, weights=BSAst)
mean_hc_whole <- max(3, 8.600001*(mean_va**0.53))
fixed_hc <- hc * mean_hc_whole/mean_hc
return(fixed_hc)
}

##############################################

calcfixedHR <- function(hr){
mean_hr <- weighted.mean(hr, weights=BSAst)
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

#Calculate local metabolic rate by work [W]
calcMworklocal <- function(bmr, par){
mwork_all <- (par-1) * bmr
mwf <- c(
  0, 0, 0.091, 0.08, 0.129,
  0.0262, 0.0139, 0.005, 0.0262, 0.0139, 0.005,
  0.2010, 0.0990, 0.005, 0.2010, 0.0990, 0.005)
mwork <- mwork_all * mwf
return(mwork)
}

#############################################

calcSUMbf <- function(bf_cr, bf_ms, bf_fat, bf_sk, bf_ava_hand, bf_ava_foot){
co = 0
co <- co + bf_cr.sum()
co <- co + bf_ms.sum()
co <- co + bf_fat.sum()
co <- co + bf_sk.sum()
co <- co + 2*bf_ava_hand
co <- co + 2*bf_ava_foot
return(co)
}

#############################################

calcHeatloss <- function(t, p, met){
res_sh = 0.0014 * met * (34 - t) #sensible heat
res_lh = 0.0173 * met * (5.87 - p) #latent heat
return (list(res_sh =res_sh,res_lh=res_lh ))
}

############################################

calcgetlts <- function(ta){
  return(2.418*1000)
}

############################################

calcSUMm <- function(mbase, mwork, mshiv, mnst){
  qcr = mbase[1]
  qms = mbase[2]
  qfat = mbase[3]
  qsk = mbase[4]
  Dict = calcIndexOrder()
  IDict = Dict$indexDict
  for(i  in seq_along(bodyNames)){
    for (bn in seq_along(bodyNames)){
      if (!is.null( IDict[[bn]][["muscle"]])) {
        qms[i] <-  qms[i] + mwork[i] + mshiv[i]
        }
      else {
        qcr[i] <-  qcr[i] + mwork[i] + mshiv[i]
        }
    }
  }
  qcr <- qcr + mnst
  return(list(qcr=qcr, qms=qms, qfat=qfat, qsk=qsk))
}

###############################################
calcConductance <- function(height=1.72, weight=74.43, equation="dubois", fat =15){

 if (fat < 12.5){
  cdt_cr_sk <- c(
      1.341, 0.930, 1.879, 1.729, 2.370,
      1.557, 1.018, 2.210, 1.557, 1.018, 2.210,
      2.565, 1.378, 3.404, 2.565, 1.378, 3.404)
}
 else if (fat < 17.5){
      cdt_cr_sk = c(
        1.311, 0.909, 1.785, 1.643, 2.251,
        1.501, 0.982, 2.183, 1.501, 0.982, 2.183,
        2.468, 1.326, 3.370, 2.468, 1.326, 3.370)
 }
 else if (fat < 22.5){
   cdt_cr_sk = c(
        1.282, 0.889, 1.698, 1.563, 2.142,
        1.448, 0.947, 2.156, 1.448, 0.947, 2.156,
        2.375, 1.276, 3.337, 2.375, 1.276, 3.337)}
 else if (fat < 27.5){
  cdt_cr_sk = c(
        1.255, 0.870, 1.618, 1.488, 2.040,
        1.396, 0.913, 2.130, 1.396, 0.913, 2.130,
        2.285, 1.227, 3.304, 2.285, 1.227, 3.304)}
 else{ #fat >= 27.5
      cdt_cr_sk = c(
        1.227, 0.852, 1.542, 1.419, 1.945,
        1.346, 0.880, 1.945, 1.346, 0.880, 1.945,
        2.198, 1.181, 3.271, 2.198, 1.181, 3.271
      )}

    cdt_cr_ms = rep(0, 17)  # core to muscle [W/K]
    cdt_ms_fat = rep(0, 17)  # muscle to fat [W/K]
    cdt_fat_sk = rep(0, 17)  # fat to skin [W/K]

    # Head and Pelvis consists of 65MN's conductances
    cdt_cr_ms[1] = 1.601  # Head
    cdt_ms_fat[1] = 13.222
    cdt_fat_sk[1] = 16.008
    cdt_cr_ms[5] = 3.0813  # Pelvis
    cdt_ms_fat[5] = 0.3738
    cdt_fat_sk[5] = 41.4954

    # vessel to core
    # The shape is a cylinder.
    # It is assumed that the inner is vascular radius, 2.5mm and the outer is
    # stolwijk's core radius.
    # The heat transer coefficient of the core is assumed as the Michel's
    # counter-flow model 0.66816 [W/(m･K)].
    cdt_ves_cr = c(
      0, 0, 0, 0, 0,
      0.586, 0.383, 1.534, 0.586, 0.383, 1.534,
      0.810, 0.435, 1.816, 0.810, 0.435, 1.816)
    #superficial vein to skin
    cdt_sfv_sk = c(
      0, 0, 0, 0, 0,
      57.735, 37.768, 16.634, 57.735, 37.768, 16.634,
      102.012, 54.784, 24.277, 102.012, 54.784, 24.277)

    # art to vein (counter-flow) [W/K]
    # The data has been derived Mitchell's model.
    # THe values = 15.869 [W/(m･K)] * the segment lenght [m]
    cdt_art_vein = c(
      0, 0, 0, 0, 0,
      0.537, 0.351, 0.762, 0.537, 0.351, 0.762,
      0.826, 0.444, 0.992, 0.826, 0.444, 0.992)

    # Changes values by body size based on the standard body.
    wr = calcWeightrate(weight)
    bsar = calcBSArate(height, weight, equation)
    # Head, Neck (Sphere shape)
    cdt_cr_sk[1:2] = cdt_cr_sk[1:2]* wr/bsar
    cdt_cr_ms[1:2]  = cdt_cr_ms[1:2]*wr/bsar
    cdt_ms_fat[1:2] =cdt_ms_fat[1:2] *wr/bsar
    cdt_fat_sk[1:2] = cdt_fat_sk[1:2]*wr/bsar
    cdt_ves_cr[1:2] =cdt_ves_cr[1:2] *wr/bsar
    cdt_sfv_sk[1:2] = cdt_sfv_sk[1:2]*wr/bsar
    cdt_art_vein[1:2] = cdt_art_vein[1:2]* wr/bsar
    # Others (Cylinder shape)
    cdt_cr_sk[3:17] = cdt_cr_sk[3:17]*bsar^2/wr
    cdt_cr_ms[3:17] = cdt_cr_ms[3:17]*bsar^2/wr
    cdt_ms_fat[3:17] = cdt_ms_fat[3:17]*bsar^2/wr
    cdt_fat_sk[3:17] = cdt_fat_sk[3:17]*bsar^2/wr
    cdt_ves_cr[3:17] = cdt_ves_cr[3:17]*bsar^2/wr
    cdt_sfv_sk[3:17] = cdt_sfv_sk[3:17]*bsar^2/wr
    cdt_art_vein[3:17] = cdt_art_vein[3:17]*bsar^2/wr

   
    cdt_whole = matrix( rep( 0, len=numNodes*numNodes), nrow = numNodes)
    for(i  in seq_along(bodyNames)){
      for(bn  in seq_along(bodyNames)){
        indexof <- idict[[bodyNames[bn]]]
        
        # Common
        cdt_whole[indexof[["artery"]], indexof[["vein"]]] = cdt_art_vein[i]  # art to vein
        cdt_whole[indexof[["artery"]], indexof[["core"]]] = cdt_ves_cr[i]  # art to cr
        cdt_whole[indexof[["vein"]], indexof[["core"]]] = cdt_ves_cr[i]  # vein to cr
        
        # Only limbs
        if (i >= 5){
          cdt_whole[indexof[["sfvein"]], indexof[["skin"]]] = cdt_sfv_sk[i] }# sfv to sk
        
        # If the segment has a muscle or fat layer
        if (!is.null( indexof[["m]uscle"]])){ # or not indexof["fat"] is None
        cdt_whole[indexof[["core"]], indexof[["muscle"]]] = cdt_cr_ms[i]  # cr to ms
        cdt_whole[indexof[["muscle"]], indexof[["fat"]]]= cdt_ms_fat[i]  # ms to fat
        cdt_whole[indexof[["fat"]], indexof[["skin"]]] = cdt_fat_sk[i]  # fat to sk
        }
        else{
          cdt_whole[indexof[["core"]], indexof[["skin"]]] = cdt_cr_sk[i] } # cr to sk
      }
    } 
    
    cdt_whole = cdt_whole + t(cdt_whole)

    return(cdt_whole)
  }

#############################################################################################################################################
