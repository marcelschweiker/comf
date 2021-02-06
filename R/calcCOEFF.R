#' @title Calculation of Coefficients for aPMV, ePMV, aPTS, ePTS 
#' @description The functions \code{calcCOEFF} calculate the coefficients necessary for apmv, epmv, apts, and epts based on a given dataset with actual comfort votes. \code{calcapCoeff} calculates lambda the adaptive coefficients for apmv, \code{calcepCoeff} calculates e the expectancy factor for epmv, \code{calcasCoeff} calculates lambda the adaptive coefficients for apts, \code{calcesCoeff} calculates e the expectancy factor for epts.
#' @aliases calcCOEFF
#' @aliases calcapCoeff
#' @aliases calcasCoeff
#' @aliases calcesCoeff
#' @aliases calcepCoeff
#' @usage calcapCoeff(lsCond)
#' @usage calcepCoeff(lsCond)
#' @usage calcasCoeff(lsCond)
#' @usage calcesCoeff(lsCond)
#' @param lsCond a list with vectors for the necessary variables (see details) .
#' @note For \code{calcapCoeff} and \code{calcepCoeff}, lsCond should contain the following variables: ta, tr, vel, rh, clo, met, wme, asv (see \code{\link{createCond}} for details). In case one or more of these variables are not included in the list, standard values will be used.
#' @note For \code{calcasCoeff} and \code{calcesCoeff}, lsCond should contain the following variables: ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, asv (see \code{\link{createCond}} for details). In case one or more of these variables are not included in the list, standard values will be used.
#' @returns \code{calcCOEFF} returns the adaptive coefficient lambda or expectancy factor depending on its call.
#' @examples ## Note. Due to random generated asv values. The values for the coefficients will not be meaningful.
#' ## Create sample data
#' ta  <- 20:24      # vector with air temperature values
#' tr  <- ta         # vector with radiant temperature values
#' vel <- rep(.1,5)  # vector with air velocities
#' rh  <- rep(50,5)  # vector with relative humidity values
#' clo <- rep(1.0,5) # vector with clo values
#' met <- rep(1.1,5) # vector with metabolic rates
#' asv <- rnorm(5)   # vector with actual sensation votes
#' 
#' lsCond <- as.list(data.frame(ta,tr,vel,rh,clo,met,asv))
#' 
#' ## Calculate coefficients
#' 
#' calcapCoeff(lsCond)
#' calcepCoeff(lsCond)
#' calcasCoeff(lsCond)
#' calcesCoeff(lsCond)
#' 
#' ## use coefficients to calculate apmv
#' lsCond$apCoeff[1] <- calcapCoeff(lsCond)$apCoeff
#' calcComfInd(lsCond, request="apmv")
#' @author Marcel Schweiker.
#' @seealso see also \code{\link{calcaPMV}}, \code{\link{calcePMV}}, \code{\link{calcPtsa}}, \code{\link{calcPtse}}
#' @references Coefficients are calculated based on Gao, J.; Wang, Y. and Wargocki, P. Comparative analysis of modified PMV models and set models to predict human thermal sensation in naturally ventilated buildings Building and Environment, 2015, 92, 200-208.
#' @references The apmv concept was introduced by Yao, R.; Li, B. and Liu, J. A theoretical adaptive model of thermal comfort - Adaptive Predicted mean Vote (apmv) Building and Environment, 2009, 44, 2089-2096.
#' @references The epmv concept was introudced by Fanger, P. and Toftum, J. Extension of the PMV model to non-air-conditioned buildings in warm climates Energy and Buildings, 2002, 34, 533-536.
#' @export



calcepCoeff <- function(lsCond){
  
  pmv     <- calcComfInd(lsCond, request = "epCoeff")
  pmv     <- cutTSV(pmv$epCoeff)
  pmv     <- as.numeric(as.character(pmv))
  amv     <- lsCond$asv
  amvpmv  <- pmv * amv
  pmvpmv  <- pmv * pmv
  sumA    <- sum(amvpmv)
  sumP    <- sum(pmvpmv)
  epCoeff <- sumA / sumP
  data.frame(epCoeff = epCoeff)
}
#' @export
calcapCoeff <- function(lsCond){
  
  pmv <- calcComfInd(lsCond, request = "apCoeff")
  pmv <- cutTSV(pmv$apCoeff)
  pmv <- as.numeric(as.character(pmv))
  amv <- lsCond$asv
  
  if (sum(which(pmv == 0)) > 0){pmv[which(pmv == 0)] <- NA}
  if (sum(which(amv == 0)) > 0){amv[which(amv == 0)] <- NA}
  
  df <- data.frame(pmv, amv)
  df <- na.omit(df)
  
  amvi <- 1 / (df$amv)
  pmvi <- 1 / (df$pmv)
  sumA <- sum(amvi)
  sumP <- sum(pmvi)
  
  apCoeff <- (sumA-sumP) / length(lsCond$asv)
  data.frame(apCoeff = apCoeff)
}

#' @export
calcesCoeff <- function(lsCond){
  
  pts <- calcComfInd(lsCond, request = "esCoeff")
  pts <- cutTSV(pts$esCoeff)
  pts <- as.numeric(as.character(pts))
  amv <- lsCond$asv
  
  amvpts <- pts*amv
  ptspts <- pts*pts
  
  sumA <- sum(amvpts)
  sumP <- sum(ptspts)
  
  esCoeff <- sumA/sumP
  data.frame(esCoeff = esCoeff)
}

#' @export
calcasCoeff <- function(lsCond){
  
  pts <- calcComfInd(lsCond, request = "asCoeff")
  pts <- cutTSV(pts$asCoeff)
  pts <- as.numeric(as.character(pts))
  amv <- lsCond$asv
  
  if (sum(which(pts == 0)) > 0){pts[which(pts == 0)] <- NA}
  if (sum(which(amv == 0)) > 0){amv[which(amv == 0)] <- NA}
  
  df <- data.frame(pts, amv)
  df <- na.omit(df)
  
  amvi <- 1 / (df$amv)
  ptsi <- 1 / (df$pts)
  sumA <- sum(amvi)
  sumP <- sum(ptsi)
  
  asCoeff <- (sumA - sumP) / length(lsCond$asv)
  data.frame(asCoeff = asCoeff)
}

