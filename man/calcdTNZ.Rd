\name{calcdTNZ}
\alias{calcdTNZ}
\alias{dTNZ}
\alias{dtnz}
\title{
Calculating dTNZ, the Distance from the Thermoneutral Zone
}
\description{
\code{calcdTNZ} calculates the distance from the thermoneutral zone, either skin temperature or room air related. 
}
\usage{
calcdTNZ(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh, 
deltaT = 0.1, fBasMet = "rosa", fSA = "duBois")
}
\arguments{
  \item{ht}{
a numeric value presenting body height in [cm] 
}
  \item{wt}{
a numeric value presenting body weight in [kg] 
}
  \item{age}{
a numeric value presenting the age in [years] 
}
  \item{gender}{
a numeric value presenting sex (female = 1, male = 2)
}
  \item{clo}{
a numeric value presenting clothing insulation level in [clo]
}
  \item{vel}{
a numeric value presenting air velocity in [m/s] 
}
  \item{tskObs}{
a numeric value presenting actual mean skin temperature in [degree C]  
}
  \item{taObs}{
a numeric value presenting air temperaturein [degree C] 
}
  \item{met}{
a numeric value presenting metabolic rate (activity related) in [met] 
}
  \item{rh}{
a numeric value presenting realtive humidity in [%] 
}
  \item{deltaT}{
a numeric value presenting the resolution of the matrix to be used
}
  \item{fBasMet}{
a string presenting the method of calculating basal metbolic rate. Needs to be one of "rosa", "harris", "miflin", or "fixed". Fixed will result in the value of 58.2 W/m2.
}
  \item{fSA}{
a string presenting the method of calculating the surface area. Needs to be one of "duBois" or "mosteller".
}
}
\value{
\code{calcdTNZ} returns a dataframe with the columns dTNZ, dTNZTs, dTNZTa. Thereby
\item{dTNZ }{The absolute distance to the centroid of the thermoneutral zone}
\item{dTNZTs }{Relative value of distance assuming skin temperature to be dominant for sensation}
\item{dTNZTa }{Relative value of distance assuming ambient temperature to be dominant for sensation}
}
\references{
Kingma, B. R., Schweiker, M., Wagner, A. and van Marken Lichtenbelt, W. D. Exploring the potential of a biophysical model to understand thermal sensation Proceedings of 9th Windsor Conference: Making Comfort Relevant Cumberland Lodge, Windsor, UK, 2016.

Kingma, B. and van Marken Lichtenbelt, W. Energy consumPtion in buildings and female thermal demand Nature Clim. Change, 2015, 5, 1054 - 1056.

Kingma, B. R.; Frijns, A. J.; Schellen, L. and van Marken Lichtenbelt, W. D. Beyond the classic thermoneutral zone Temperature, 2014, 1, 142-149.
}
\author{
Marcel Schweiker and Boris Kingma
}
\note{
In case one of the variables is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
}
\seealso{
see also \code{\link{calcComfInd}}
}
\examples{
## Calculate all values
calcdTNZ(171, 71, 45, 1, .6, .12, 37.8, 25.3, 1.1, 50) 
}
\keyword{ manip }
