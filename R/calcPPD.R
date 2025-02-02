#' @title Predicted Percentage of Dissatisfied (PPD)
#' @description Function to calculate Predicted Percentage of Dissatisfied (PPD).
#' @aliases ppd
#' @usage calcPPD(ta, tr, vel, rh, clo=.5, met=1, wme=0, basMet=58.15)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @param basMet a numeric value presenting basal metabolic rate [w/m2]
#' @details The PPD is an index that establishes a quantitative prediction of the percentage of thermally dissatisfied people determined from PMV.
#' @details Note that the adjustments in the value for basMet need to be made with great cautiousness as the PMV calculation is an empirical model and might not be valid for other values of basMet than the one commonly used.
#' @returns PPD - Predicted Percentage of Dissatisfied occupants in [\%]
#' @examples calcPPD(25, 25, 0.3, 50, 0.5, 1)
#' @author Code implemented in to R by Marcel Schweiker. Further contribution by Sophia Mueller and Shoaib Sarwar.
#' @references Fanger (1970) Thermal Comfort Analysis and Applications in Environmental Engineering McGraw-Hill, New York.
#' @references ISO 7730 Ergonomics of the thermal environment analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005.
#' @export


calcPPD <- function(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, basMet = 58.15) {
  calcPMVPPD(ta, tr, vel, rh, clo, met, wme, basMet)$ppd
}
