#' Calibration data for SET
#'
#' Data from ASHRAE 55-2013 to calibrate values given by SET model
#'
#' @docType data
#'
#' @usage data(dfASHRAETableG11)
#'
#' @format A data frame with 22 rows and 11 variables:
#' \describe{
#'  \item{\code{ta}}{a numeric vector of air temperature [degree C]}
#' 	\item{\code{taF}}{a numeric vector of air temperature [degree F]}
#'  \item{\code{tr}}{a numeric vector of radiant temperature [degree C]}
#' 	\item{\code{trF}}{a numeric vector of radiant temperature [degree F]}
#' 	\item{\code{vel}}{a numeric vector of indoor air velocity [m/s]}
#' 	\item{\code{velFPM}}{a numeric vector of indoor air velocity [fpm]}
#'  \item{\code{rh}}{a numeric vector of relative humidity [\%]}
#' 	\item{\code{met}}{a numeric vector of metabolic rate [MET]}
#'  \item{\code{clo}}{a numeric vector of clothing insulation level [CLO]}
#'  \item{\code{set}}{a numeric vector of standard effective temperature (SET) [degree C]}
#'  \item{\code{setF}}{a numeric vector of standard effective temperature (SET) [degree F]}
#' }
#'
#' @keywords datasets
#'
#' @references ASHRAE Standard 55-2013. Thermal environmental conditions for human occupancy. American society of heating, Refrigerating and Air-Conditioning Engineering, Atlanta, USA, 2013.
#'
#' @examples
#' data(dfASHRAETableG11)
#' head(dfASHRAETableG11)
"dfASHRAETableG11"
