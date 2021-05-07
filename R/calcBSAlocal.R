#' @title Calculate Local Body Surface Area
#' @description Function to  Calculate local body surface area (BSA) [m2].
#' @aliases calcbsalocal
#' @aliases bsalocal
#' @usage calcBSAlocal(height=1.72, weight=74.43, equation="dubois")
#' @usage calcbsalocal(height=1.72, weight=74.43, equation="dubois")
#' @usage bsalocal(height=1.72, weight=74.43, equation="dubois")
#' @param height Body height. The default is 1.72 in [m]
#' @param weight Body height. The default is 74.43 [kg]
#' @param equation The equation name of bsa calculation. The default is "dubois" in [str]
#' @details Choose a name from "dubois","takahira", "fujimoto", or "kurazumi". All these equations are defined in helpJOS functions.
#' @returns Local body surface area (BSA) [m2].
#' @examples calcBSAlocal()
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export


calcBSAlocal <- function(height=1.72, weight=74.43, equation="dubois"){

# The BSA ratio to the standard body (1.87m2)
bsa_rate <- calcBSArate(height, weight, equation)
bsa <- BSAst * bsa_rate
return(bsa)

}


