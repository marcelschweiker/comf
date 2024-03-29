#' PMV based on Adaptive Thermal Heat Balance Framework for the Standard Model
#' 
#' aliases athb ATHB
#' @description \code{calcATHBstandard} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBstandard(trm, ta, tr, vel, rh, met)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#'
#' @return \code{calcATHBstandard} PMV value adapted through the ATHB approach with standard model
#'
#' @references 
#' Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' Schweiker (2022) <doi:10.1111/ina.13018>
#' 
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso see also \code{\link{calcComfInd}}, \code{link{calcATHBpts}}, \code{link{calcATHBset}},
#' @seealso \code{link{calcATHBpmv2015}}
#' @export
#'
#' @examples calcATHBstandard(20, 25, 25, .1, 50, 1.1)

calcATHBstandard <- function(trm, ta, tr, vel, rh, met){
  
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (-0.1716848 - 0.0004853 * trm + 0.0817623 * metAdpt - 0.0052730 * trm * metAdpt)

  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)$Lraw
  
  # predicted thermal sensation vote 
  PTSVATHBpmv <- 1.484304 + 0.0276035 * LAdpt - 0.9602187 * metAdpt - 0.03421333 *
    trm + 0.002971073 * LAdpt * metAdpt + 0.0002264348 * LAdpt * trm +
    0.01869608 * metAdpt * trm + 0.0002264348 * LAdpt * trm - 0.0002909158 * 
    LAdpt * metAdpt * trm
  PTSVATHBpmv
}
