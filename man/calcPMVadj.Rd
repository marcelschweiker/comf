\name{calcpmvadj}
\alias{calcpmvadj}
\alias{calcaPMV}
\alias{calcePMV}
\alias{pmvadj}
\alias{aPMV}
\alias{ePMV}
\alias{apmv}
\alias{epmv}
\title{
Calculating Adjusted Predicted Mean Votes
}
\description{
\code{calcpmvadj} calculates predicted mean votes (pmv) adjusted for cooling effect of elevated air speed, through the adaptive coefficient, or the expectancy factor.
}
\usage{
calcpmvadj(ta, tr, vel, rh, clo, met, wme = 0)
calcaPMV(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, apCoeff)
calcePMV(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, epCoeff, asv)
}
\arguments{
  \item{ta}{
a numeric value presenting air temperature in [degree C]  
}
  \item{tr}{
a numeric value presenting mean radiant temperature in [degree C]  
}
  \item{vel}{
a numeric value presenting air velocity in [m/s] 
}
  \item{rh}{
a numeric value presenting relative humidity [\%]
}
  \item{clo}{
a numeric value presenting clothing insulation level in [clo] 
}
  \item{met}{
a numeric value presenting metabolic rate in [met] 
}
  \item{wme}{
a numeric value presenting external work in [met] 
}
  \item{apCoeff}{
adaptive coefficient lambda
}
  \item{epCoeff}{
expectancy factor e
}
  \item{asv}{
actual thermal sensation vote coded with 0 = neutral, hot = +3, cold = -3.
}
}
\details{
\code{apCoeff} can be derived using \code{calcapCoeff}.

\code{epCoeff} can be derived using \code{calcepCoeff}.

\code{calcePMV} requires the actual sensation vote related to the physical data as it is required to alter the metabolic rate.
}
\value{
\code{calcpmvadj} returns the predicted mean vote adjusted for the cooling effect of elevated air speed.

\code{calcaPMV} returns the predicted mean vote adjusted through the adaptive coefficients.

\code{calcePMV} returns the predicted mean vote adjusted through the expectancy factor.
}
\references{
pmvadj is based on ASHRAE standard 55-2013. Thermal environmental conditions for human occupancy. American society of heating, Refrigerating and Air-Conditioning Engineering, Atlanta, Usa, 2013

apmv is based on Yao, R., Li, B. and Liu, J. A theoretical adaptive model of thermal comfort - Adaptive Predicted mean Vote (aPMV) Building and Environment, 2009, 44, 2089-209

epmv is based on Fanger, P. and Toftum, J. Extension of the PMV model to non-air-conditioned buildings in warm climates Energy and Buildings, 2002, 34, 533-536
}
\author{
Marcel Schweiker. The function used for bisection method is taken from a forum entry and with permission by ravi Varadhan.
}
\note{
In case one of the variables is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
}
\seealso{
see also \code{\link{calcComfInd}}, \code{\link{calcapCoeff}}, \code{\link{calcepCoeff}}
}
\examples{
## Note. Due to random generated asv values. The values for the coefficients will not be meaningful.
## Create sample data
ta  <- 20:24     # vector with air temperature values
tr  <- ta         # vector with radiant temperature values
vel <- rep(.1,5)  # vector with air velocities
rh  <- rep(50,5)  # vector with relative humidity values
clo <- rep(1.0,5) # vector with clo values
met <- rep(1.1,5) # vector with metabolic rates
asv <- rnorm(5)   # vector with actual sensation votes

lsCond <- as.list(data.frame(ta,tr,vel,rh,clo,met,asv))

## Calculate coefficient apCoeff for data set
apCoeff <- calcapCoeff(lsCond)

## calculate apmv
apmv <- NULL
for (i in 1:length(ta)){
apmv[i] <- calcaPMV(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], apCoeff = apCoeff)$apmv
}
apmv

}
\keyword{ manip }