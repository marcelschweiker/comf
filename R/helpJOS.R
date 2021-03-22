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

calcPreferredTemp <- function(va=0.1, rh=50, met=1, clo=0){
  to = 28
  for(i  in seq(1,100)){
    vpmv = calcPMV(to, to, va, rh, met, clo)
    if(abs(vpmv) < 0.001) break
    else to = to - vpmv/3
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

run <- function(dtime=60, passive=False, output=True){
  tcr <- Tcr()
  tsk <- Tsk()
  
  # Convective and radiative heat transfer coefficient [W/K.m2]
  posture <- "standing"
  va <- rep(0.1,17)
  ta <- rep(28.8,17)
  hc <- fixedHc(convCoef(posture, va, ta, tsk), va)
  hr <- fixedHr(radCoef(posture))
  
  # Operarive temp. [oC], heat and evaporative heat resistance [K/W], [kPa/W]
  tr <- rep(28.8,17)
  clo <- rep(0, 17)
  iclo <- rep(0.45, 17)
  to <- operativeTemp(ta, tr, hc, hr)
  rT <- dryR(hc, hr, clo)
  rEt <- wetR(hc, clo, iclo) 
  
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
    bfAvaHand = 0
    bfAvaFoot = 0
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
  indexDict = list()
  
  for(key in c("Head", "Pelvis")){
    tempDict = list("artery"= 1, "vein"= 1, "sfvein"= NA, "core"= 1, "muscle"= 1, 
                    "fat"= 1, "skin"= 1)
    indexDict[[key]]= tempDict
  }
  
  for (key in c("Neck", "Chest", "Back")) {
    tempDict = list("artery"= 1, "vein"= 1, "sfvein"= NA, "core"= 1, 
                    "muscle"= NA, "fat"= NA, "skin"= 1)
    indexDict[[key]]= tempDict
  }
  
  for(key in bodyNames()[-(1:5)]){
    tempDict = list("artery"= 1, "vein"= 1, "sfvein"= 1, "core"= 1, 
                    "muscle"= NA, "fat"= NA, "skin"= 1)
    indexDict[[key]]= tempDict
  }
  indexDict["CB"] = 0
  orderCount = 1
  for(bn in bodyNames()){
    for(ln in layerNames()){
      if(!is.na(indexDict[[bn]][[ln]])){
        indexDict[[bn]][[ln]] = orderCount
        orderCount = orderCount + 1
      }
    }
  }
  list(indexDict, orderCount)
}

##############################################

calcBodyTemp <- function(){
  numNodes <- calcIndexOrder()[[2]]
  bodyTemp <- rep.int(36, 85)
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

fixedHc <- function(posture, va, ta, tsk){
  
}

##############################################

fixedHr <- function(hr){
  
}

##############################################

convCoef <- function(hc, va) {
  
}

##############################################

radCoef <- function(posture){
  
}

##############################################

operativeTemp <- function(ta, tr, hc, hr){
  
}

##############################################

dryR <- function(hc, hr, clo){
  
}

##############################################

wetR <- function(hc, clo, iclo = 0.45, lewisRate = 16.5){
  
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
  return(mapply(c, c(1,2), c(2,3), SIMPLIFY=FALSE))
}

##############################################

sumM <- function(mbase, mwork, mshiv, mnst){
 list(qcr = c(1,2), qms = c(1,2), qfat = c(1,2), qsk = c(1,2)) 
}

##############################################

crmsfatBloodflow <- function(mwork, mshiv, height=1.72, weight=74.43, 
                             equation="dubois", age=20, ci=2.59){
  list(bfCr = c(1,2), bfMs = c(1,2), bfFat = c(1,2)) 
}

##############################################

antoine <- function(ta){
  rep(1, 17)
}
