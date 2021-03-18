#' @title Calculate the ratio of basal blood flow
#' @description Function to Calculate the ratio of basal blood flow (BFB) of the standard body (290 L/h).
#' @aliases calcbfbrate
#' @aliases bfbrate
#' @usage calcBFBrate(height=1.72, weight=74.43, age=20, ci=2.59, equation="dubois")
#' @usage calcbfbrate(height=1.72, weight=74.43, age=20, ci=2.59, equation="dubois")
#' @usage bfbrate(height=1.72, weight=74.43, age=20, ci=2.59, equation="dubois")
#' @param height Body height. The default is 1.72 in [m]
#' @param weight Body height. The default is 74.43 in [kg]
#' @param age Age. The default is 20 in [years]
#' @param ci  Cardiac index. The default is 2.59 in [L/min/m2]
#' @param equation The equation name of bsa calculation. The default is "dubois" in [str]
#' @details Choose a name from "dubois","takahira", "fujimoto", or "kurazumi". All these equations are defined in helpJOS functions.
#' @returns  Basal blood flow rate (bfb_rate)
#' @examples calcBFBrate()
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export

calcBFBrate <- function(height=1.72, weight=74.43, age=20, ci=2.59, equation="dubois"){


  ci <- ci * 60  # Change unit [L/min/㎡] to [L/h/㎡]

  # Decrease of BFB by aging
  if (age < 50){
    ci <- ci * 1}
  else if (age < 60){
    ci <- ci * 0.85}
  else if (age < 70){
    ci <- ci * 0.75}
  else{
    ci <- ci * 0.7}  # age >= 70


  bfb_all <- ci * calcBSArate(height, weight, equation) * sum(BSAst())  # [L/h]
  bfb_rate <- bfb_all / 290
  return(bfb_rate)
}

