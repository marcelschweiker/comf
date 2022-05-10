#' PMV based on Adaptive Thermal Heat Balance Framework
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmv} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBpmv(trm, ta, tr, vel, rh, met)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#'
#' @return \code{calcATHBpmv} PMV value adapted through the ATHB appoach
#' 
#' @details All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.
#' 
#' @references 
#' Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' Schweiker (2022) <doi:10.1111/ina.13018>
#' 
#' @author Marcel Schweiker
#' @seealso see also \code{\link{calcComfInd}}, \code{link{calcATHBpts}}, \code{link{calcATHBset},
#' \code{link{calcATHBpmv2015}}
#' @export
#'
#' @examples calcATHBpmv(20, 25, 25, .1, 50, 1.1)

calcATHBpmv <- function(trm, ta, tr, vel, rh, met){
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (-0.172 - 0.000485 * trm + 0.0818 * metAdpt - 0.000527*trm*metAdpt)
  
  # adapted thermal load according to Fanger’s PMV mode
  LApdt <- calcPMVL(ta, tr, vel, rh, cloAdpt, metAdpt)

  # predicted thermal sensation vote PTSV(ATHB-PMV)
}
