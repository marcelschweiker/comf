#######################################################
#run all these functions before running JOS3-functions
#######################################################

#Global Variables 
height <- 1.72
weight <- 74.43
fat <- 15
sex <- "male"
age <- 20
ci <- 2.59
bmrEquation <- "harris-benedict"
bsaEquation <- "dubois"
exOutput <- NULL

# Body surface area [m2]
bsaRate <- calcBSArate(height, weight, bsaEquation)
# Body surface area rate [-]
bsa <- calcBSAlocal(height, weight, bsaEquation)
# Basal blood flow rate [-]
bfbRate <- calcBFBrate(height, weight, bsaEquation, age, ci)
# Thermal conductance [W/K]
cdt <- calcConductance(height, weight, bsaEquation, fat)
# Thermal capacity [J/K]
cap <- calcCapacity(height, weight, bsaEquation, age, ci)

# Set point temp [oC]
setptCr <- rep(37, 17)
setptSk <- rep(34, 17)

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

# Default values of input condition
ta <- rep(28.8, 17)
tr <- rep(28.8, 17)
rh <- rep(50, 17)
va <- rep(0.1, 17)
clo <- rep(0, 17)
iclo <- rep(0.45, 17)
par <- 1.25  # Physical activity ratio
posture <- "standing"
hc <- NULL
hr <- NULL
exQ <- rep(0, numNodes)
t <- 0 # Elapsed time
cycle <- 0 # Cycle time
modelName <- "JOS3"
options <- list(
  nonshivering_thermogenesis = TRUE,
  cold_acclimated = FALSE,
  shivering_threshold = FALSE,
  limit_dshivdt = FALSE,
  bat_positive = FALSE,
  ava_zero = FALSE,
  shivering = FALSE)
preSHIV <- 0 # reset
history <- c()

# Reset setpoint temperature
dictout <- resetSetpt()
history <- append(history, dictout)

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

run <- function(dtime=60, passive=False, output=True){
  #  Run a model for a once and get model parameters.
  
  tcr <- Tcr()
  tsk <- Tsk()
  
  # Convective and radiative heat transfer coefficient [W/K.m2]
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
  for(i  in seq_along(bodyNames())){
    for (bn in seq_along(bodyNames())){
      if (!is.null( IDict[[bn]][["muscle"]])) { qms[i] <-  qms[i] + mwork[i] + mshiv[i]}
      else {qcr[i] <-  qcr[i] + mwork[i] + mshiv[i]}
      qcr <- qcr + mnst
    }
  }
  return(list(qcr=qcr, qms=qms, qfat=qfat, qsk=qsk))
}

###############################################
