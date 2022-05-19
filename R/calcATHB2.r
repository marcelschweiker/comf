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
#' 
#' @references 
#' Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' Schweiker (2022) <doi:10.1111/ina.13018>
#' 
#' @author Marcel Schweiker
#' @seealso see also \code{\link{calcComfInd}}, \code{link{calcATHBpts}}, \code{link{calcATHBset}},
#' @seealso \code{link{calcATHBpmv2015}}
#' @export
#'
#' @examples calcATHBpmv(20, 25, 25, .1, 50, 1.1)

calcATHBpmv <- function(trm, ta, tr, vel, rh, met){
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (-0.17168 - 0.000485 * trm + 0.08176 * metAdpt - 0.00527*trm*metAdpt)
  
  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)

  # predicted thermal sensation vote 
  PTSVATHBpmv <- 1.484 + 0.0276 * LAdpt - 0.9602 * metAdpt - 0.0342 * trm + 
  0.0002264 * LAdpt * trm + 0.018696 * metAdpt *trm - 0.0002909 * LAdpt * metAdpt * trm
  
  PTSVATHBpmv
}
