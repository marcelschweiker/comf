#' PMV based on Adaptive Thermal Heat Balance Framework of multiple Models
#'
#' aliases athb ATHB calcATHBPmvModel1, calcATHBPmvModel2, calcATHBPmvModel3
#' @description \code{calcATHBpmv} calculates the PMV based on adaptive thermal heat balance framework
#' @description based on the newest version (2022)
#'
#' @usage calcATHBpmv(trm, ta, tr, vel, rh, met,
#' coolingStrategyBuilding = 'naturallyventilated',
#' buildingTypeSimple = 'office', seasonSimple = 'spring')
#'
#' @param trm - Running mean outdoor temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param tr - a numeric value presenting mean radiant temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param rh - a numeric value presenting relative humidity [\%]
#' @param met - a numeric value presenting metabolic rate in [met]
#' @param buildingTypeSimple - simple building type. Value can be among Multifamily housing,
#' Office as a string
#' @param coolingStrategyBuilding - the process in which the building was
#' ventilated. Value can be among Mixed Mode','Naturally Ventilated' as a String
#' @param seasonSimple - season. Value can be among 'Spring','Summer', 'Winter' as a String
#'
#' @return \code{calcATHBpmv} an array of PMV values of different models adapted
#' through the ATHB appoach
#'
#' @references
#' Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' Schweiker (2022) <doi:10.1111/ina.13018>
#'
#' @author Code implemented in to R by Shaomi Rahman. Further contribution by Marcel Schweiker.
#' @seealso see also \code{\link{calcComfInd}}, \code{link{calcATHBpts}}, \code{link{calcATHBset}},
#' @seealso \code{link{calcATHBpmv2015}}, \code{link{calcATHBPmvModel1}},
#' \code{link{calcATHBPmvModel2}}, \code{link{calcATHBPmvModel3}}
#' @export
#'
#' @examples calcATHBpmv(
#'   20, 25, 25, .1, 50, 1.1, "naturallyventilated", "office",
#'   "spring"
#' )
calcATHBpmv <- function(trm, ta, tr, vel, rh, met, coolingStrategyBuilding =
                          "naturallyventilated", buildingTypeSimple = "office",
                        seasonSimple = "spring") {
  PTSVATHBstandard <- calcATHBstandard(trm, ta, tr, vel, rh, met)
  PTSVATHBx <- calcATHBx(
    trm, ta, tr, vel, rh, met, buildingTypeSimple,
    coolingStrategyBuilding, seasonSimple
  )
  c(PTSVATHBstandard, PTSVATHBx)
}
