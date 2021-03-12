#run all these functions before running JOS3-functions

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
  bodyTemp = rep.int(36, 85)
  
}


##############################################

calcIndexByLayer <- function(layer){
  outIndex <- c()
  for(bn in bodyNames()){
    for(ln in layerNames()){
      
    }
  }
    
}