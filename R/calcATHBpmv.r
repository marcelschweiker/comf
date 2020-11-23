#' Calculation of Adaptive Thermal Heat Balance Indices with PMV
#' 
#' calcATHB calculates three different indices related to the adaptive thermal heat balance framework. This function returns the pmv value adapted through the ATHB approach
#' 
#' @usage calcATHBpmv(trm, psych, ta, tr, vel, rh, met, wme)
#' 
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param psych	- factor related to fixed effect on perceived control
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param wme - a numeric value presenting external work in [met]
#' 
#' @details All variables must have the same length 1. For the calculation of several values use function calcComfInd.
#' 
#' @return pmv value adapted through the ATHB approach
#' 
#' @examples 
#' calcATHBpmv(20, 0, 25, 25, .1, 50, 1.1, 0)
#' 
#' @export

calcATHBpmv <- function(trm, psych, ta, tr, vel, rh, met, wme){

  #calc with clo according to fix effects
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

	comfortData <- data.frame(calcPMVPPD(ta, tr, vel, rh, cloeq, metadapt, wme))
	giveDat <- with(comfortData, get("pmv"))
	giveDat
}
