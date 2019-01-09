\name{calcHumidity}
\alias{calcHumidity}
\alias{calcDewp}
\alias{calcEnth}
\alias{calcHumx}
\alias{calcMixR}
\alias{calcRH}
\alias{calcSVP}
\alias{calcVP}
\alias{calcVapourpressure}
\title{
Calculating Various Humidity Related Values
}
\description{
This set of functions calculates different humidity related values based on the given entities.
}
\usage{
calcDewp(ta, rh)
calcEnth(ta, rh, pb)
calcHumx(ta, rh)
calcMixR(ta, rh, pb)
calcRH(ta, mr, pb)
calcSVP(ta)
calcVP(ta, mr, pb)
calcVapourpressure(ta, rh)
}
\arguments{
  \item{ta}{
a numeric value or vector presenting air temperature in [degree C] 
}
  \item{rh}{
a numeric value or vector presenting relative humidity in [\%], except for \code{calcVapourpressure}, where it must be in decimal (e.g. 0.5)
}
  \item{mr}{
a numeric value or vector presenting the mixIng ratio in [g/kg]
}
  \item{pb}{
a numeric value or vector presenting barometric pressure in [torr]
}
}
\details{
The length of the arguments must be either the same or they must have the length one and one common second length.
}
\value{
\code{calcDewp} returns the dew point temperature in [degree C] 

\code{calcEnth} returns a single value or a vector of values of enthalpy in [J]

\code{calcHumx} returns a single value or a vector of values of the humidex of air [ ]

\code{calcMixR} returns a single value or a vector of mixIng ratio in [g/kg]

\code{calcRH} returns a single value or a vector of relative humidities in [\%]

\code{calcSVP} returns a single value or a vector of saturation vapor pressure in [kpa]

\code{calcVP} returns a single value or a vector of vapor pressure in [kpa]

\code{calcVapourpressure} returns a single value or a vector of vapor pressure in [kpa]

}
\references{
For references related to humidex, see:
Rajib Ranaa, Brano Kusya, Raja Jurdaka, Josh Wallb and Wen Hua, Feasibility analysis of using humidex as an indoor thermal comfort predictor, Energy and Buildings 64 (2013) 17-25.

Masterton, J. M., and Richardson, F. A., Humidex a method of quantifying humandiscomfort due to excessive heat and humidity, clI 1-79. Downsview, Ont: Environment Canada. Atmosheric Environment Service, 1979.
}
\author{
Michael Kleber (code and documentation), Marcel Schweiker (documentation)
}
\examples{
## Calc single value of absolute humidity
ta <- 25
rh <- 50
calcMixR(ta, rh, 760)

## Calc set of values of absolute humidity
ta <- 25:30
rh <- 50
calcMixR(ta, rh, 760)

## Calculating dew point temperature with single values for ta and rh
calcDewp(25, 50)

## Calculating dew point temperature with a vector of values for ta and a single value for rh
calcDewp(25:29, 50)

## Calc single value of enthalpy
ta <- 25
rh <- 50
calcEnth(ta, rh, 760)

## Calc set of values of enthalpy
ta <- 25:30
rh <- 50
calcEnth(ta, rh, 760)
}
\keyword{ manip }