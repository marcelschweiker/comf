#' Callibration data for Tre, SWtotg, Dlimtre, Dlimloss50, Dlimloss95
#'
#' Data from ISO 7933 Appendix F to calibrate values given by the proposed model
#'
#' @docType data
#'
#' @usage data(dfISO7933AppF)
#'
#' @format A data frame with 10 rows and 16 variables:
#' \describe{
#'  \item{\code{accl}}{a numeric vector of state of acclimatised subject, 100 if acclimatised, 0 otherwise [-]}
#'	\item{\code{posture}}{a numeric vector of posture of subject, posture = 1 sitting, =2 standing, =3 crouching [-]}
#'  \item{\code{Ta}}{a numeric vector of air temperature [degree C]}
#'	\item{\code{Pa}}{a numeric vector of partial water vapour pressure [kPa]}
#'  \item{\code{Tr}}{a numeric vector of mean radiant temperature [degree C]}
#'  \item{\code{Va}}{a numeric vector of air velocity (m/s)}
#'  \item{\code{Met}}{a numeric vector of metabolic rate (W/(m*m))}
#'  \item{\code{Icl}}{a numeric vector of static thermal insulation (clo)}
#'  \item{\code{THETA}}{a numeric vector of angle between walking direction and wind direction (degrees)}
#'  \item{\code{Walksp}}{a numeric vector of walking speed (m/s)}
#'  \item{\code{Duration}}{a numeric vector of the duration of the work sequence (min)}
#'  \item{\code{Tre}}{a numeric vector of final rectal temperature (degree C)}
#'  \item{\code{SWtotg}}{a numeric vector of total water loss (g)}
#'  \item{\code{Dlimtre}}{a numeric vector of maximum allowed exposition time for heat storage (min)}
#'  \item{\code{Dlimloss50}}{a numeric vector of maximum water loss for protection of an average person (g)}
#'  \item{\code{Dlimloss95}}{a numeric vector of maximum water loss for protection of 95\% of the working people (g)}
#' }
#'
#' @keywords datasets
#'
#' @references ISO 7933 Ergonomics of the thermal environment - Analytical determination and interpretation of heat stress using calculation of the predicted heat strain 2004
#'
#' @examples
#' data(dfISO7933AppF)
#' head(dfISO7933AppF)
"dfISO7933AppF"