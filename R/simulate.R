#' Execute JOS3 model.
#' 
#' @description Function to execute JOS3 model 
#' @usage simulate(times, dtime, output)
#' @param times Number of loops of a simulation
#' @param dtime Time delta [sec]. The default is 60
#' @param output If you don't record paramters, set False. The default is True. It is optional.
#' @details executes the run function which executes the JOS3 model number of times as times parameter. 
#' @return None
#' @examples simulate(3, 60, TRUE)
#' @examples simulate(2, 60)
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
#'
simulate <- function(times, dtime = 60, output = TRUE){
  for(t in seq(1, times)){
    t = t + dtime
    cycle <- cycle + 1
    dictdata <- run(dtime = dtime, output = output)
    if(output){
      history <- history + dictdata
    }
  }
}