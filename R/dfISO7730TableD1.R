#' Callibration data for SET
#'
#' Data from ISO 7730 to callibrate values given by SET model
#'
#' @docType data
#'
#' @usage data(dfISO7730TableD1)
#'
#' @format A data frame with 13 rows and 8 variables:
#' \describe{
#'  \item{\code{ta}}{a numeric vector of air temperature [degree C]}
#'  \item{\code{tr}}{a numeric vector of radiant temperature [degree C]}
#'	\item{\code{vel}}{a numeric vector of indoor air velocity [m/s]}
#'  \item{\code{rh}}{a numeric vector of relative humidity [\%]}
#'	\item{\code{met}}{a numeric vector of metabolic rate [MET]}
#'  \item{\code{clo}}{a numeric vector of clothing insulation level [CLO]}
#'  \item{\code{pmv}}{a numeric vector of Predicted mean vote (PMV)}
#'  \item{\code{ppd}}{a numeric vector of Predicted percentage dissatisfied (PPD)} 
#' }
#'
#' @keywords datasets
#'
#' @references ISO 7730 Ergonomics of the thermal environment analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005.
#'
#' @examples
#' data(dfISO7730TableD1)
#' head(dfISO7730TableD1)
"dfISO7730TableD1"