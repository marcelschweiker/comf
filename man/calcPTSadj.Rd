\name{calcPtsadj}
\alias{calcPtsadj}
\alias{calcPtsa}
\alias{calcPtse}
\alias{Ptsadj}
\alias{Ptsa}
\alias{Ptse}
\alias{ptsadj}
\alias{ptsa}
\alias{ptse}
\title{
Calculating the Predicted Thermal Sensation based on 2-Node Model adjusted for Adaptation or Expectancy
}
\description{
\code{calcpts} are two functions calculating predicted thermal sensation votes (pts) based on Gagges two-node model and adjusted either through the adaptive coefficient lambda or the expectancy factor e.
}
\usage{
calcPtsa(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, asCoeff)
calcPtse(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, esCoeff)
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
  \item{pb}{
a numeric value presenting barometric pressure in [torr] or [mmHg]
}
  \item{ltime}{
a numeric value presenting exposure time in [minutes] 
}
  \item{ht}{
a numeric value presenting body height in [cm] 
}
  \item{wt}{
a numeric value presenting body weight in [kg] 
}
  \item{tu}{
a numeric value presenting the turbulence intensity (value does not affect outcome)
}
  \item{asCoeff}{
adaptive coefficient lambda
}
  \item{esCoeff}{
expectancy factor e
}
}
\details{
\code{asCoeff} can be derived using \code{calcasCoeff}.

\code{esCoeff} can be derived using \code{calcesCoeff}.

\code{calcPtse} requires the actual sensation vote related to the physical data as it is required to alter the metabolic rate.
}
\value{
\code{calcPtsa} returns the predicted thermal sensation vote adjusted through the adaptive coefficients.

\code{calcPtse} returns the predicted thermal sensation vote adjusted through the expectancy factor.
}
\references{
Gao, J., Wang, Y. and Wargocki, P. Comparative analysis of modified pmv models and set models to predict human thermal sensation in naturally ventilated buildings, Building and Environment, 2015, 92, 200-208.
}
\author{
Marcel Schweiker
}
\note{
In case one of the variables is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
}

\seealso{
see also \code{\link{calcComfInd}}, \code{\link{calcasCoeff}}, \code{\link{calcesCoeff}}
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

## Calculate coefficient asCoeff for data set
asCoeff <- calcasCoeff(lsCond)

## calculate ptsa
ptsa <- NULL
for (i in 1:length(ta)){
ptsa[i] <- calcPtsa(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], asCoeff = asCoeff)$ptsa
}
ptsa

}
\keyword{ manip }
