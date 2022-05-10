#' @title Calculate WRMS and CLDS signals of thermo-regulation
#' @description Function to Calculate WRMS and CLDS signals of thermo-regulation
#' @aliases calcerrorsig
#' @usage calcERRORsig(err_cr=0, err_sk=0)
#' @param err_cr Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param err_sk Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @returns  dataframe of WRMS and CLDS signals.
#' @examples calcERRORsig()
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export

calcERRORsig <- function(err_cr=0, err_sk=0){

# SKINR
receptor = c(
  0.0549, 0.0146, 0.1492, 0.1321, 0.2122,
  0.0227, 0.0117, 0.0923, 0.0227, 0.0117, 0.0923,
  0.0501, 0.0251, 0.0167, 0.0501, 0.0251, 0.0167)

# wrms signal
wrm <- pmax(err_sk, 0)
wrm <- wrm * receptor
wrms <- sum(wrm)
# clds signal
cld = pmax(err_sk, 0)
cld <- cld * (-receptor)
clds = sum(cld)

return(list(wrms=wrms, clds=clds))
}
