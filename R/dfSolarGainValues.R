#' Dataset with Different Combinations of Inputs to Calculate Solar Gain
#'
#' @docType data
#'
#' @keywords internal
#' @usage data(dfSolarGainValues)
#'
#' @format A data frame with 384 rows and 10 variables:
#' \describe{
#'  \item{\code{solAlt}}{a numeric value presenting solar altitude, degrees from horizontal in [degree C]}
#' 	\item{\code{solAzi}}{a numeric value presenting solar azimuth, degrees clockwise from North in [degree C]}
#'  \item{\code{solRadDir}}{a numeric value presenting direct-beam solar radiation [W/m2]}
#' 	\item{\code{solTrans}}{a numeric value presenting total solar transmittance. Ranges from 0 to 1}
#' 	\item{\code{fSvv}}{a numeric value presenting fraction of sky vault exposed to body, ranges from 0 to 1}
#' 	\item{\code{fBes}}{a numeric value presenting fraction of the possible body surface exposed to sun, ranges from 0 to 1}
#'  \item{\code{asw}}{a numeric value presenting the average short-wave absorptivity of the occupant. Set to 0.7 by default}
#' 	\item{\code{posture}}{Default 'seated' list of available options 'standing', 'supine' or 'seated'}
#'  \item{\code{erf}}{Solar gain to the human body using the Effective Radiant Field [W/m2]}
#'  \item{\code{delMrt}}{Delta mean radiant temperature [Degree C]}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(dfSolarGainValues)
#' head(dfSolarGainValues)
"dfSolarGainValues"
