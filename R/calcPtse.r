#' Predicted Thermal Sensation based on 2-Node Model adjusted
#' for Expectancy
#'
#' @aliases calcPtse ptse calcptse Ptse
#' @description
#' \code{calcPtse} calculates Predicted Thermal Sensation based on the
#' 2-Node-Model by Gagge et al. and adjusts its output according to expectancy
#' factor
#'
#' @usage
#' calcPtse(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, pb = 760,
#'                      ltime = 60, ht = 171, wt = 70, tu = 40, esCoeff)
#'
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @param pb a numeric value presenting barometric pressure in [torr] or [mmHg]
#' @param ltime a numeric value presenting exposure time in [minutes]
#' @param ht a numeric value presenting body height in [cm]
#' @param wt a numeric value presenting body weight in [kg]
#' @param tu a numeric value presenting turbulence intensity in [\%]
#' @param esCoeff a numeric values presenting expectancy factor [-]
#'
#' @details
#' All variables must have the same length 1. For the calculation of several
#' values use function \code{calcComfInd}. The value of \code{obj} defines
#' whether the function will use the version presented in ASHRAE 55-2013 for
#' adjustment of pmv (obj = "pmvadj"), or the original code by Gagge to calculate
#' set (obj = "set"). In the version presented in ASHRAE 55-2013, the lines of
#' code related to self-generated convection is deleted. Therefore, a difference
#' can only be seen at higher values of met.
#'
#' @return
#' \code{calcPtse} returns a dataframe containing the Predicted Thermal
#' Sensation value
#'
#' @note
#' In case one of the variables is not given, a standard value will be taken
#' from a list (see \code{\link{createCond}} for details).
#'
#' @author
#' The code for \code{calc2Node} is based on the code in BASIC and C++ presented
#' by Fountain and Huizenga (1995). The translation into R-language and comparison
#' with ASHRAE 55-2013 conducted by Marcel Schweiker.
#'
#' @references
#' ASHRAE Standard 55-2013. Thermal environmental conditions for human occupancy.
#' American society of heating, Refrigerating and Air-Conditioning Engineering,
#' Atlanta, USA, 2013.

#' Fountain & Huizenga (1995) A thermal sensation model for use by the
#' engineering profession ASHRAE RP-781 Final report.
#'
#' Gagge, Fobelets & Berglund (1986) A standard predictive index
#' of human response to the thermal environment, ASHRAE transactions, 92 (2B), 709-731.

#' Coefficients are calculated based on Gao, Wang & Wargocki (2015) <doi:10.1016/j.buildenv.2015.04.030>
#' The aPMV concept was introduced by Yao, Li & Liu (2009) <doi:10.1016/j.buildenv.2009.02.014>
#' The ePMV concept was introudced by Fanger & Toftum (2002) <doi:10.1016/S0378-7788(02)00003-8>
#' @seealso see also \code{\link{calcComfInd}} and \code{\link{calc2Node}}
#' @export
#'
#' @examples
#' ## Using several rows of data:
#' ta <- c(20, 22, 24)
#' tr <- ta
#' vel <- rep(.15, 3)
#' rh <- rep(50, 3)
#' esCoeff <- 0.5
#'
#' maxLength <- max(sapply(list(ta, tr, vel, rh), length))
#' ptse <- sapply(seq(maxLength), function(x) {
#'   calcPtse(ta[x], tr[x], vel[x],
#'     rh[x],
#'     esCoeff = esCoeff
#'   )
#' })
calcPtse <- function(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, pb = 760,
                     ltime = 60, ht = 171, wt = 70, tu = 40, esCoeff) {
  pmv <- as.numeric(calcPMVPPD(ta, tr, vel, rh, clo, met, wme)[1])
  met <- ifelse(pmv > 0, met * (1 + pmv * (-.067)), met)
  set <- calc2Node(ta, tr, vel, rh, clo, met, wme, NULL, pb, ltime, ht, wt, tu, obj = "set")[2]
  ptse <- .25 * set - 6.03
  ptse <- esCoeff * ptse
  names(ptse) <- "ptse"
  data.frame(ptse = ptse)
}
