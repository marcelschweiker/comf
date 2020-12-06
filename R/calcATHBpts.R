#' Calculation of Adaptive Thermal Heat Balance Indices with PTS
#' 
#' @description \code{calcATHB} calculates three different indices related to the adaptive thermal heat balance framework according to PTS
#' 
#' @usage calcATHBpts(trm, psych, ta, tr, vel, rh, met, wme, pb, ltime, ht, wt)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param psych - factor related to fixed effect on perceived control
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param wme - a numeric value presenting external work in [met]
#' @param pb - a numeric value presenting barometric pressure in [torr] or [mmHg]
#' @param ltime - a numeric value presenting exposure time in [minutes]
#' @param ht - a numeric value presenting body height in [cm]
#' @param wt - a numeric value presenting body weight in [kg]
#'
#' @return \code{calcATHBpts} value adapted through the ATHB appoach
#' 
#' @details All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.
#' 
#' @references 
#' Schweiker, M. & Wagner, A. A framework for an adaptive thermal heat balance model (ATHB), Building and Environment, 2015, 94, 252 - 262
#' Schweiker, M. & Wagner, A. Exploring potentials and limitations of the adaptive thermal heat balance framework Proceedings of 9th Windsor Conference: making comfort relevant Cumberland Lodge, Windsor, UK, 2016
#' 
#' @author Marcel Schweiker
#' @seealso see also \code{\link{calcComfInd}}
#' @export
#'
#' @examples calcATHBpts(20, 0, 25, 25, .1, 50, 1.1, 0, 760, 60, 171, 70)
calcATHBpts <- function(trm, psych, ta, tr, vel, rh, met, wme, pb, ltime, ht, wt){
  
  set <- calcATHBset(trm, psych, ta, tr, vel, rh, met, wme, pb, ltime, ht, wt)
  ATHBpts <- .25 * set - 6.03
  #names(ATHBpts) <- "ATHBpts"
  data.frame(ATHBpts)
  
}
