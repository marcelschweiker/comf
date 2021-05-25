#' Set operative temperature under PMV=0 environment
#' 
#' @description Function to reset operative temperature under PMV=0 environment 
#' @usage resetSetpt()
#' @param None
#' @details At first, resets the value of met, to, RH, va, icl, PAR, ava_zero. 
#' Then returns dictout with contains all the basic variable of JOS setup. 
#' @return dictout a dictionary of all the basic variable in JOS setup
#' @examples resetSetpt()
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
#'

resetSetpt <- function(height, weight, age, sex, bmrEquation, options, posture,
                       ta, bsa, va, tr, clo, iclo, bsaEquation, ci, par, fat, rh,
                       exOutput){
  # Set operative temperature under PMV=0 environment. 
  # Metabolic rate at PAR = 1.25
  # 1 met = 58.15 W/m2
  met <- calcMETbasal(height, weight, age, sex, bmrEquation) *  1.25 / 58.15 / sum(bsa)
  to <- rep(calcPreferredTemp(met=met), 17)
  RH <- 50
  icl <- 0
  
  options[["ava_zero"]] = TRUE
  for(i in seq(1,10)){
    dictout <- run(dtime=60000, passive=TRUE, posture=posture, va=va, ta=ta, 
                   options = options, tr = tr, clo = clo, iclo = iclo, 
                   height = height, bsaEquation = bsaEquation, weight = weight, 
                   age = age, ci = ci, sex = sex, par = par, fat = fat, rh=rh, 
                   bmrEquation = bmrEquation, bsa = bsa, to = to, exOutput= exOutput)
  }
  
  # Set new setpoint temperatures
  setptCr <- Tcr()
  setptSk <- Tsk()
  options[["ava_zero"]] = FALSE
  
  dictout
}
