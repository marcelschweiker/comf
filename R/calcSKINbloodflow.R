#' @title Calculate skin blood flow rate (BFsk) [L/h].
#' @description Function to calculate skin blood flow rate (BFsk) [L/h].
#' @aliases calcEvap,calcEvaporation
#' @param err_cr Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param err_sk Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param height Body height [m]. The default is 1.72.
#' @param weight Body weight [kg]. The default is 74.43.
#' @param equation equation
#' @param age Age [years]. The default is 20.
#' @param ci Cardiac index [L/min/m2]
#' @returns BFsk : array of Skin blood flow rate [L/h].
#' @example 
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
calcSKINbloodflow <- function(err_cr, err_sk,
                   height=1.72, weight=74.43, equation="dubois", age=20, ci=2.59){


error <- calcERRORsig(err_cr, err_sk)  # Thermoregulation signals
wrms <- error$wrms
clds <- error$clds

# BFBsk
bfb_sk <- c(
  1.754, 0.325, 1.967, 1.475, 2.272,
  0.91, 0.508, 1.114, 0.91, 0.508, 1.114,
  1.456, 0.651, 0.934, 1.456, 0.651, 0.934)
# SKIND
skin_dilat <- c(
  0.0692, 0.0992, 0.0580, 0.0679, 0.0707,
  0.0400, 0.0373, 0.0632, 0.0400, 0.0373, 0.0632,
  0.0736, 0.0411, 0.0623, 0.0736, 0.0411, 0.0623)
# SKINC
skin_stric <- c(
  0.0213, 0.0213, 0.0638, 0.0638, 0.0638,
  0.0213, 0.0213, 0.1489, 0.0213, 0.0213, 0.1489,
  0.0213, 0.0213, 0.1489, 0.0213, 0.0213, 0.1489)

sig_dilat <- (100.5*err_cr[1]) + (6.4*(wrms-clds))
sig_stric <- (-10.8*err_cr[1]) + (-10.8*(wrms-clds))
sig_dilat <- max(sig_dilat, 0)
sig_stric <- max(sig_stric, 0)
# Signal decrement by aging
if (age < 60){
  sd_dilat <- c(rep(1,17))
  sd_stric <- c(rep(1,17))
}
else{ #age >= 60
  sd_dilat <- c(
    0.91, 0.91, 0.47, 0.47, 0.31,
    0.47, 0.47, 0.47, 0.47, 0.47, 0.47,
    0.31, 0.31, 0.31, 0.31, 0.31, 0.31)
  sd_stric <- c(rep(1,17))
}


bf_sk <- (1 + skin_dilat * sd_dilat * sig_dilat) /
(1 + skin_stric * sd_stric * sig_stric) * bfb_sk * 2^(err_sk/6)

bfbr <- calcBFBrate(height, weight, equation, age, ci)
bf_sk <- bf_sk * bfbr
return(bf_sk)
}
