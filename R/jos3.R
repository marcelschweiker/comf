jos3 <- function(height=1.72, weight=74.43, fat=15, age=20, sex="male", ci=2.59, 
                 bmrEquation="harris-benedict", bsaEquation="dubois", 
                 exOutput=NULL, modelName = "JOS"){
  
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
  setptCr <- rep(37, 17) #core
  setptSk <- rep(34, 17) #skin
  
  # Initial body temp [oC]
  bodyTemp <- rep(36, numNodes)
  
  # Default values of input condition
  to <- NULL
  ta <- rep(28.7654, 17)
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
  t <- Sys.time() # Elapsed time
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
  
  object <- list(height = height, weight = weight, fat = fat, sex = sex, age = age, 
                 ci = ci, bmrEquation = bmrEquation, bsaEquation = bsaEquation, 
                 exOutput = exOutput, bsaRate = bsaRate, bsa = bsa,
                 bfbRate = bfbRate, cdt = cdt, cap = cap, setptCr = setptCr, 
                 setptSk = setptSk, bodyTemp = bodyTemp, to = to, ta = ta, 
                 tr = tr, rh = rh, va = va, clo = clo, iclo = iclo, par = par, 
                 posture = posture, hc = hc, hr = hr, exQ = exQ, t = t, 
                 cycle = cycle, modelName = modelName, options = options, 
                 history = history)
  
  # Reset setpoint temperature
  result <- resetSetpt(object)
  dictout <- result$dictout
  object <- result$object
  object$history <- append(object$history, dictout)
  list(dictout = dictout, object = object)
}
