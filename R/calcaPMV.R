#' @title Calculating Adjusted Predicted Mean Votes
#' @description Function to calculate predicted mean votes (pmv) adjusted for cooling effect of elevated air speed, through the adaptive coefficient by using the function PMVPPD.
#' @aliases aPMV
#' @aliases apmv
#' @usage calcaPMV(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, apCoeff)
#' @usage aPMV(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, apCoeff)
#' @usage apmv(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, apCoeff)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @param apCoeff adaptive coefficient lambda
#' @details \code{apCoeff} can be derived using \code{calcapCoeff}.
#' @returns \code{calcaPMV} returns the predicted mean vote adjusted through the adaptive coefficients.
#' @examples
#' ## Note. Due to random generated asv values. The values for the coefficients will not be meaningful.
#' ## Create sample data
#' ta  <- 20:24     # vector with air temperature values
#' tr  <- ta         # vector with radiant temperature values
#' vel <- rep(.1,5)  # vector with air velocities
#' rh  <- rep(50,5)  # vector with relative humidity values
#' clo <- rep(1.0,5) # vector with clo values
#' met <- rep(1.1,5) # vector with metabolic rates
#' asv <- rnorm(5)   # vector with actual sensation votes

#' lsCond <- as.list(data.frame(ta,tr,vel,rh,clo,met,asv))

#' ## Calculate coefficient apCoeff for data set
#' apCoeff <- calcapCoeff(lsCond)

#' ## calculate apmv
#' apmv <- NULL
#' for (i in 1:length(ta)){
#'  apmv[i] <- calcaPMV(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], apCoeff = apCoeff)$apmv}
#' apmv
#' @author Code implemented in to R by Marcel Schweiker. Further contribution by Sophia Mueller and Shoaib Sarwar.
#' @note In case one of \code{apCoeff} is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
#' @seealso \code{\link{calcComfInd}}, \code{\link{calcapCoeff}}
#' @references apmv is based on Yao, R., Li, B. and Liu, J. A theoretical adaptive model of thermal comfort - Adaptive Predicted mean Vote (aPMV) Building and Environment, 2009, 44, 2089-209
#' @export


calcaPMV <- function(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, apCoeff){

  pmv  <- calcPMVPPD(ta, tr, vel, rh, clo, met, wme)[1]
  apmv <- pmv / (1 + apCoeff * pmv)
  names(apmv) <- "apmv"
  data.frame(apmv = apmv)

}

aPMV <- calcaPMV
apmv <- calcaPMV
