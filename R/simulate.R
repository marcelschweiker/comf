#' Execute JOS3 model.
#' @aliases simulate Simulate jos3 Jos3 JOS3 resetSetpt resetsetpt
#' @usage simulate(times, dtime, output, object)
#' @description Function to simulate JOS3 model. The returned object can be saved and used for another simulation. 
#' @param times Number of loops of a simulation[numerical]
#' @param dtime Time delta. The default is 60[sec]
#' @param output If you don't record paramters, set False. The default is True. It is optional.[Boolean]
#' @param object The instance of JOS3 model in a list form on which JOS3 will be executed.[list] 
#' @details executes the run function which executes the JOS3 model number of times as times parameter. At first, the JOS3 model must be created to create a model with jos3() function.
#' @return the object with all the updated variables after the simulation. [list]
#' @examples model1 <- jos3()$object
#' @examples simulate(2, 60, TRUE, model)
#' @examples model2 <- jos3(bmrEquation <- "harris-benedict_origin")$object
#' @examples simulate(times = 2, object = model)
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{resetSetpt}}
#' @seealso \code{\link{jos3}}
#' @export
#'
simulate <- function(times, dtime = 60, output = TRUE, object){
  for(t in seq(1, times)){
    t = t + dtime
    object$cycle <- object$cycle + 1
    result <- run(dtime = dtime, output = output, object = object)
    dictdata <- result$dictout
    object <- result$updatedVariables
    if(output){
      object$history <- append(object$history, dictdata)
    }
  }
  object
}
