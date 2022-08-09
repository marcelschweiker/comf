#' Dataset with Different Combinations of Inputs and Outputs for calcNewDTS calcDPD calcStaticPartDTS
#'
#' @docType data
#' @usage data(output)
#'
#' @format A data frame with 370 rows and 16 variables:
#' \describe{
#'  \item{\code{X}}{a numeric value presenting index number}
#'  \item{\code{TA}}{a numeric value presenting air temperature in [degree C]}
#'	\item{\code{TR}}{a numeric value presenting mean radiant temperature in [degree C]}
#'	\item{\code{RH}}{a numeric value presenting relative humidity [\%]}
#'  \item{\code{VEL}}{a numeric value presenting air velocity in [m/s]}
#'  \item{\code{CLO}}{a numeric value presenting clothing insulation level}
#'  \item{\code{MET}}{a numeric value presenting metabolic rate}
#'	\item{\code{PMV}}{a numeric value presenting metabolic rate}
#'	\item{\code{Tskin}}{a numeric value presenting skin temparature [degree C]}
#'	\item{\code{Tcore}}{a numeric value presenting core temparature [degree C]}
#'	\item{\code{dTskin}}{a numeric value presenting gradient skin temparature [degree C]}
#'	\item{\code{dTcore}}{a numeric value presenting gradient core temparature [degree C]}
#'	\item{\code{STS}}{a numeric value presenting the output of calcStaticPartDTS}
#'	\item{\code{DTS}}{a numeric value presenting the output of calcNewDTS}
#'	\item{\code{DPD}}{a numeric value presenting the output of calcDPD}

#' }
#'
#' @keywords datasets
#'
#' @seealso see also \code{\link{calcStaticPartDTS}} \code{\link{calcNewDTS}} \code{\link{calcDPD}}
#' @examples
#' data(output)
#' head(output)
"dfDPD"
