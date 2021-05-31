#' Set operative temperature under PMV=0 environment
#' @aliases simulate Simulate jos3 Jos3 JOS3 resetSetpt resetsetpt
#' @description Function to reset operative temperature under PMV=0 environment. 
#' @usage resetSetpt(object)
#' @param object dictionary of all the necessary variables[list]
#' @details At first, resets the value of met, to, RH, va, icl, PAR, ava_zero. Then returns dictout with contains all the basic variable of JOS setup and the list of all the updated variables of the model. 
#' @return dictout a dictionary \code{dictout} the result and \code{updatedVariables} the updated variables of the model after reset.
#' @examples model1 = jos3()$object
#' @examples resetSetpt(model1)
#' @examples model2 <- jos3(bmrEquation <- "harris-benedict_origin")$object
#' @examples resetSetpt(model2)$object
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{resetSetpt}}
#' @seealso \code{\link{jos3}}
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
