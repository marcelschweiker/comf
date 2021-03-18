#' @title Calculate the rate of BSA to standard body
#' @description Function to calculate the ratio of BSA to the standard body.
#' @aliases calcbsarate
#' @aliases bsarate
#' @usage calcBSArate(height=1.72, weight=74.43, equation="dubois")
#' @usage calcbsarate(height=1.72, weight=74.43, equation="dubois")
#' @usage bsarate(height=1.72, weight=74.43, equation="dubois")
#' @param height Body height. The default is 1.72 in [m]
#' @param weight Body height. The default is 74.43 [kg]
#' @param equation The equation name of bsa calculation. The default is "dubois" in [str]
#' @details Choose a name from "dubois","takahira", "fujimoto", or "kurazumi". All these equations are defined in helpJOS functions.
#' @returns ratio of BSA to the standard body [-]
#' @examples calcBSArate()
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export

calcBSArate <- function(height=1.72, weight=74.43, equation="dubois"){

bsa_all <- calcBSA(height, weight, equation)
bsa_rate <- bsa_all/sum(BSAst())
return(bsa_rate)
}


