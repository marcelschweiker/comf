#' PMV based on Adaptive Thermal Heat Balance Framework
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmv} calculates the PMV based on adaptive thermal heat balance framework
#' 
#' @usage calcATHBpmv(trm, psych, ta, tr, vel, rh, met, wme)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param psych - factor related to fixed effect on perceived control
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param wme - a numeric value presenting external work in [met]
#'
#' @return \code{calcATHBpmv} PMV value adapted through the ATHB appoach
#' 
#' @details All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.
#' 
#' @references 
#' Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' Schweiker & Wagner (2016) Exploring potentials and limitations of the adaptive thermal heat balance framework Proceedings of 9th Windsor Conference: making comfort relevant Cumberland Lodge, Windsor, UK, 2016
#' 
#' @author Marcel Schweiker
#' @seealso see also \code{\link{calcComfInd}}, \code{link{calcATHBpts}}, \code{link{calcATHBset}}
#' @export
#'
#' @examples calcATHBpmv(20, 0, 25, 25, .1, 50, 1.1, 0)

calcATHBpmvNew <- function(trm, ta, tr, vel, rh, met){
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (-0.172 - 0.000485 * trm + 0.0818 * metAdpt - 0.000527*trm*metAdpt)
  
  # adapted thermal load according to Fanger’s PMV mode
  LApdt <- calcPMVL(ta, tr, vel, rh, cloAdpt, metAdpt)

  # predicted thermal sensation vote PTSV(ATHB-PMV)
}
