#' SET based on Adaptive Thermal Heat Balance Framework
#' 
#' @description Calculation of SET based on Adaptive Thermal Heat Balance framework using Gagge's 2-node model
#' 
#' @usage calcATHBset(trm, psych, ta, tr, vel, rh, met, wme = 0, pb = 760, 
#'                       ltime = 60, ht = 171, wt = 69.9)
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
#' @return ATHBset set value adapted through the ATHB appoach
#' 
#' @details All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.
#' 
#' @references 
#' Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' Schweiker & Wagner (2016) Exploring potentials and limitations of the adaptive thermal heat balance framework Proceedings of 9th Windsor Conference: making comfort relevant Cumberland Lodge, Windsor, UK, 2016
#' 
#' @author Marcel Schweiker
#' @seealso see also \code{\link{calcComfInd}}, \code{link{calcATHBpmv}}, \code{link{calcATHBpts}}
#' @export
#'
#' @examples calcATHBset(20, 0, 25, 25, .1, 50, 1.1, 0, 760, 60, 171, 70)

calcATHBset <- function(trm, psych, ta, tr, vel, rh, met, wme = 0, pb = 760, 
                      ltime = 60, ht = 171, wt = 69.9){
  
  ### calc with clo according to fix effects
  
  HFtomet <- .092
  PContoHF <- (-.549)
  tointopCon <- (-.06264)
  metadaptPhys <- .193 * HFtomet
  metadaptPCon <- PContoHF * tointopCon * HFtomet
  
  # adjustment of clothing value based on behavioural adaptation
  cloeq <- 1.252594 + trm * (-0.03023063)
  cloeq <- ifelse (cloeq > 1, 1, ifelse (cloeq < .46, .46, cloeq))
  
  # adjustment of met based on physiological adaptation
  dmetadaptPhys <- ifelse (((trm - 18) * metadaptPhys) < 0, 0, ((trm - 18) * metadaptPhys))
  
  # adjustment of met based on psychological adaptation ( variable part)
  dmetadaptpsychVar <- ifelse (((ta - 20) * metadaptPCon) < 0, 0, ((ta - 20) * metadaptPCon))
  
  # adjustment of met based on psychological adaptation (fixed part)
  dmetadaptpsychFix <- psych * PContoHF * HFtomet
  
  metadapt <- met - dmetadaptPhys + dmetadaptpsychVar + dmetadaptpsychFix
  
  comfortData <- data.frame(calc2Node(ta, tr, vel, rh, cloeq, metadapt, wme, NULL, pb, ltime, ht, wt, obj = "set"))
  giveDat <- with(comfortData, get("set"))
  giveDat
}
