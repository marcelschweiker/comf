#' Create JOS3 model.
#' @aliases simulate Simulate jos3 Jos3 JOS3 resetSetpt resetsetpt
#' @usage jos3(height, weight, fat, age, sex, ci, bmrEquation, bsaEquation, exOutput, modelName)
#' @description Function to create JOS3 model. The returned object can be saved and used for another simulation. 
#' @param height Body height. The default is 1.72[m]
#' @param weight Body weight [kg]
#' @param fat Fat rate [%]
#' @param age Age [years]
#' @param sex Sex ("male" or "female")
#' @param ci Cardiac index [L/min/m2]
#' @param bmrEquation Choose a BMR equation.
#' @param bsaEquation Choose a BSA equation.
#' @param exOutput Extra output parameters. If "all", all parameters are output.
#' @details Creates the model on which all the other functions of JOS model will be executed. Returns the result and updated variables of the model. 
#' @return the object with all the updated variables after the simulation. [list]
#' @examples model1 <- jos3()
#' @examples model2 <- jos3(bmrEquation <- "harris-benedict_origin")
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{resetSetpt}}
#' @seealso \code{\link{simulate}}
#' @export
#'
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
