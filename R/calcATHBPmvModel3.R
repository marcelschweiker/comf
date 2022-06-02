#' PMV based on Adaptive Thermal Heat Balance Framework for Model 3
#' 
#' aliases athb ATHB
#' @description \code{calcATHBpmvModel3} calculates the PMV based on adaptive thermal heat balance framework 
#' @description based on the newest version (2022)
#' 
#' @usage calcATHBpmvModel3(trm, ta, tr, vel, rh, met)
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#'
#' @return \code{calcATHBpmvModel3} PMV value adapted through the ATHB appoach with model 3
#'
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
#' @examples calcATHBpmvModel3(20, 25, 25, .1, 50, 1.1)

calcATHBpmvModel3 <- function(trm, ta, tr, vel, rh, met){
  # metabolic rate through physiological adaptation
  metAdpt <- met - (0.234 * trm) / 58.2
  
  # adapted clothing insulation level through behavioural adaptation
  cloAdpt <- 10 ^ (-0.1716848 - 0.0004853 * trm + 0.0817623 * metAdpt - 0.0052730 * trm * metAdpt)
  
  # adapted thermal load according to Fanger’s PMV mode
  LAdpt <- calcPMVPPD(ta, tr, vel, rh, cloAdpt, metAdpt, getLoad = TRUE)
  
  # predicted thermal sensation vote 
  PTSVATHBpmv <- 4.254359 + 0.01745583 * LAdpt - 0.006497189 * trm + 0.000184154 * LAdpt * trm
  PTSVATHBpmv
}
