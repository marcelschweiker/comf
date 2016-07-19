\name{calcATHB}
\alias{ATHB}
\alias{athb}
\alias{calcATHB}
\alias{calcATHBpmv}
\alias{calcATHBset}
\alias{calcATHBpts}
\alias{ATHBpmv}
\alias{ATHBset}
\alias{ATHBpts}
\alias{athbpmv}
\alias{athbset}
\alias{athbpts}
\title{
Calculation of Adaptive Thermal Heat Balance Indices
}
\description{
\code{calcATHB} calculates three different indices related to the adaptive thermal heat balance framework
}
\usage{
calcATHBpmv(trm, psych, ta, tr, vel, rh, met, wme)
calcATHBpts(trm, psych, ta, tr, vel, rh, met, wme, pb, ltime, ht, wt)
calcATHBset(trm, psych, ta, tr, vel, rh, met, wme, pb, ltime, ht, wt)
}
\arguments{
  \item{trm}{
Running mean outdoor temperature in [degree C]  
}
  \item{psych}{
factor related to fixed effect on perceived control 
}
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
}
\details{
All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.
}
\value{
Depending on the type chosen \code{calcATHB} returns the following item:
\item{ATHBpmv}{pmv value adapted through the ATHB appoach}
\item{ATHBset}{set value adapted through the ATHB appoach}
\item{ATHBpts}{pts value adapted through the ATHB appoach}
}
\references{
Schweiker, M. & Wagner, A. A framework for an adaptive thermal heat balance model (ATHB), Building and Environment, 2015, 94, 252 - 262

Schweiker, M. & Wagner, A. Exploring potentials and limitations of the adaptive thermal heat balance framework Proceedings of 9th Windsor Conference: making comfort relevant Cumberland Lodge, Windsor, UK, 2016
}
\author{
Marcel Schweiker
}

\seealso{
see also \code{\link{calcComfInd}}
}
\examples{
## Calculation of ATHBpmv
calcATHBpmv(20, 0, 25, 25, .1, 50, 1.1, 0)

## Calculation of ATHBpts
calcATHBpts(20, 0, 25, 25, .1, 50, 1.1, 0, 760, 60, 171, 70)

## Calculation of ATHBset
calcATHBset(20, 0, 25, 25, .1, 50, 1.1, 0, 760, 60, 171, 70)
}

\keyword{ manip }
