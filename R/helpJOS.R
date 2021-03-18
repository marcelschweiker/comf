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
