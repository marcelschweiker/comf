#' Calculating Gagge's Version of Fanger's PMV based on the 2-Node-Model
#'
#' @aliases calcPMVGagge PMVGagge Gagge
#' @description 
#' \code{calcPMVGagge} calculates Gagge's Version of Fanger's PMV based on the 
#' 2-Node-Model by Gagge et al.
#' 
#' @usage 
#' calcPMVGagge(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, 
#' ltime = 60, ht = 171, wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, 
#' cstr = 0.5)
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
#' @param obj a character element, either "set" or "pmvadj"
#' @param csw a numeric value presenting the driving coefficient for regulatory sweating
#' @param cdil a numeric value presenting the driving coefficient for vasodilation
#' @param cstr a numeric value presenting the driving coefficient for vasoconstriction
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
#' \code{calcPMVGagge} returns Gagge's Version of Fanger's PMV
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
#' 
#' Fountain, M. & Huizenga, C. A thermal sensation model for use by the 
#' engineering profession ASHRAE RP-781 Final report, 1995
#'
#' Gagge, A. P., Fobelets, A. P. and Berglund, L. G. A standard predictive index 
#' of human response to the thermal environment, ASHRAE transactions, 1986, 92 (2B), 
#' 709-731.
#' 
#' @seealso see also \code{\link{calcComfInd}}
#' @export
#'
#' @examples
#' ## Using several rows of data:
#' ta <- c(20,22,24)
#' tr <- ta
#' vel <- rep(.15,3)
#' rh <- rep(50,3)
#' 
#' maxLength <- max(sapply(list(ta, tr, vel, rh), length))
#' pmvg <- sapply(seq(maxLength), function(x) { calcPMVGagge(ta[x], tr[x], vel[x], rh[x]) } ) 

calcPMVGagge <- function(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, pb = 760, 
                         ltime = 60, ht = 171, wt = 70, tu = 40, obj = "set", 
                         csw = 170, cdil = 120, cstr = .5){
  calc2Node(ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, tu, obj, csw, cdil, cstr)$pmvg
}
