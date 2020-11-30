#' @title Calculating Adjusted Predicted Mean Votes
#' @description Function to calculate predicted mean votes (pmv) adjusted for cooling effect of elevated air speed, through the expectancy factor by using the function PMVPPD.
#' @aliases ePMV
#' @aliases epmv
#' @usage calcePMV(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, epCoeff)
#' @usage ePMV(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, epCoeff)
#' @usage epmv(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, epCoeff)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @param epCoeff expectancy factor e
#' @details \code{epCoeff} can be derived using \code{calcepCoeff}.
#' @details \code{calcePMV} requires the actual sensation vote related to the physical data as it is required to alter the metabolic rate.
#' @returns \code{calcePMV} returns the predicted mean vote adjusted through the expectancy factor.
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

#' ## Calculate coefficient epCoeff for data set
#' epCoeff <- calcepCoeff(lsCond)

#' ## calculate epmv
#' epmv <- NULL
#' for (i in 1:length(ta)){
#'  epmv[i] <- calcePMV(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], epCoeff = epCoeff)$epmv}
#' epmv
#' @author Code implemented in to R by Marcel Schweiker. Further contribution by Sophia Mueller and Shoaib Sarwar.
#' @note In case one of \code{epCoeff} is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
#' @seealso see also \code{\link{calcComfInd}}, \code{\link{calcepCoeff}}
#' @references epmv is based on Fanger, P. and Toftum, J. Extension of the PMV model to non-air-conditioned buildings in warm climates Energy and Buildings, 2002, 34, 533-536
#' @export



calcePMV <- function(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, epCoeff){

  pmv <- as.numeric(calcPMVPPD(ta, tr, vel, rh, clo, met, wme)[1])
  met <- ifelse (pmv > 0, met * (1 + pmv * (-.067)), met)
  pmv <- calcPMVPPD(ta, tr, vel, rh, clo, met, wme)[1]
  epmv <- epCoeff * pmv
  names(epmv) <- "epmv"
  data.frame(epmv = epmv)

}
ePMV <- calcePMV
epmv <- calcePMV


