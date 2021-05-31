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

resetSetpt <- function(object){
  # Set operative temperature under PMV=0 environment. 
  # Metabolic rate at PAR = 1.25
  # 1 met = 58.15 W/m2
  met <- calcMETbasal(object$height, object$weight, object$age, object$sex, 
                      object$bmrEquation) *  1.25 / 58.15 / sum(object$bsa)
  object$to <- rep(calcPreferredTemp(met=met), 17)
  result <- NULL
  object$options[["ava_zero"]] <- TRUE
  
  for(i in seq(1,10)){
    result <- run(dtime=60000, passive=TRUE, output = TRUE, object)
  }
  dictout <- result$dictout
  object <- result$updatedVariables
  # Set new setpoint temperatures
  object$setptCr <- Tcr(object$bodyTemp)
  object$setptSk <- Tsk(object$bodyTemp)
  object$options[["ava_zero"]] <- FALSE
  list(dictout = dictout, object = object)
}
