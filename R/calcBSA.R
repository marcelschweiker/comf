#' @title Calculate Body Surface Area
#' @description Function to calculate body surface area (BSA) in [m2]
#' @aliases calcbsa
#' @aliases bsa
#' @usage calcBSA(height=1.72, weight=74.43, equation="dubois")
#' @usage calcbsa(height=1.72, weight=74.43, equation="dubois")
#' @usage bsa(height=1.72, weight=74.43, equation="dubois")
#' @param height Body height. The default is 1.72 in [m]
#' @param weight Body height. The default is 74.43 [kg]
#' @param equation The equation name of bsa calculation. The default is "dubois" in [str]
#' @details Choose a name from "dubois","takahira", "fujimoto", or "kurazumi". All these equations are defined in helpJOS functions.
#' @returns Body surface area (BSA) [m2]
#' @examples calcBSA()
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export

calcBSA <- function(height=1.72, weight=74.43, equation="dubois"){
  
  bsa <- NULL
  if (equation == "dubois")
  {
    bsa <- calcDubois(height, weight)}
  else if (equation == "takahira")
  {
    bsa <- calcTakahira(height, weight)}
  else if (equation == "fujimoto")
  {
    bsa <- calcFujimoto(height, weight)}
  else if (equation == "kurazumi")
  {
    bsa <- calcKurazumi(height, weight)}
  
  bsa
}