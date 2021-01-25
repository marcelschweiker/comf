#' Dataset with DIfferent Combination of Inputs to Calculate UTCI
#'
#' @docType data
#' @keywords internal
#' @usage data(utciValues)
#'
#' @format A data frame with 81 rows and 5 variables:
#' \describe{
#'  \item{\code{ta}}{a numeric value presenting air temperature in [degree C]}
#'	\item{\code{tr}}{a numeric value presenting mean radiant temperature in [degree C]}
#'  \item{\code{vel}}{a numeric value presenting air velocity in [m/s]}
#'	\item{\code{rh}}{a numeric value presenting relative humidity [\%]}
#'	\item{\code{utci_value}}{a numeric value presenting the UTCI value}
#' }
#'
#' @keywords datasets internal
#' 
#' @examples
#' data(utciValues)
#' head(utciValues)
"utciValues"
