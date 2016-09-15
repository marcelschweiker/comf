\name{calc2Node}
\alias{calc2Node}
\alias{calcET}
\alias{calcSET}
\alias{calcTSens}
\alias{calcDisc}
\alias{calcPD}
\alias{calcPS}
\alias{calcPTS}
\alias{2Node}
\alias{Gagge}
\alias{ET}
\alias{SET}
\alias{TSens}
\alias{Disc}
\alias{PD}
\alias{PS}
\alias{PTS}
\alias{et}
\alias{set}
\alias{tsens}
\alias{disc}
\alias{pd}
\alias{ps}
\alias{pts}
\alias{calcPMVGagge}
\alias{calcPMVStar}
\alias{calcSkinWettedness}
\title{
Calculating Comfort Indices based on the 2-Node-Model
}
\description{
\code{calc2Node} calculates ET, SET, TSENS, DISC, PD, PS and PTS based on the 2-Node-Model by Gagge et al.
}
\usage{
calc2Node(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5, varOut = "else")
calcET(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcSET(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcTSens(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcDisc(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcPD(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcPS(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcPTS(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcPMVGagge(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcPMVStar(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0, pb = 760, ltime = 60, ht = 171,
 wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = 0.5)
calcSkinWettedness(ta, tr, vel, rh, clo = .5, met = 1, wme = 0, pb = 760, ltime = 60, 
 ht = 171, wt = 70, tu = 40, obj = "set", csw = 170, cdil = 120, cstr = .5, 
 varOut="skinWet")
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
a numeric value presenting turbulence intensity in [\%] 
}
  \item{obj}{
a character element, either "set" or "pmvadj". 
}
  \item{csw}{
a numeric value presenting the driving coefficient for regulatory sweating
}
  \item{cdil}{
a numeric value presenting the driving coefficient for vasodilation
}
  \item{cstr}{
a numeric value presenting the driving coefficient for vasoconstriction
}
  \item{varOut}{
a string value either "else" for normal output of SET or "skinWet" to report value of skin wettedness.
}
}
\details{
All variables must have the same length 1. For the calculation of several values use function \code{calcComfInd}.

The value of \code{obj} defines whether the function will use the version presented in ASHRAE 55-2013 for adjustment of pmv (obj = "pmvadj"), or the original code by Gagge to calculate set (obj = "set"). In the version presented in ASHRAE 55-2013, the lines of code related to self-generated convection is deleted. Therefore, a difference can only be seen at higher values of met.
}
\value{
\code{calc2Node} returns a data.frame with the following items:
\item{et}{Effective temperature}
\item{set}{standard effective temperature}
\item{tsens}{Predicted thermal sensation }
\item{disc}{Predicted discomfort}
\item{ps}{Predicted percentage satisfied with the level of air movement}
\item{pd}{Predicted percentage dissatisfied due to draft}
\item{pts}{Predicted thermal sensation vote based on set}
\item{pmvg}{Gagge's version of Fanger's PMV}
\item{pmvstar}{Same as Fanger's PMV except that dry is calculated using SET* rather than the operative temperature}
The other functions return a single index, e.g. code(calcSET) returns the standard effective temperature.
}
\references{
ASHRAE Standard 55-2013. Thermal environmental conditions for human occupancy. American society of heating, Refrigerating and Air-Conditioning Engineering, Atlanta, USA, 2013.

Fountain, M. & Huizenga, C. A thermal sensation model for use by the engineering profession ASHRAE RP-781 Final report, 1995

Gagge, A. P., Fobelets, A. P. and Berglund, L. G. A standard predictive index of human response to the thermal environment, ASHRAE transactions, 1986, 92 (2B), 709-731.
}
\author{
The code for \code{calc2Node} is based on the code in BASIC and C++ presented by Fountain and Huizenga (1995). The translation into R-language and comparison with ASHRAE 55-2013 conducted by Marcel Schweiker.
}
\note{
In case one of the variables is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
}
\seealso{
see also \code{\link{calcComfInd}}, \code{\link{calcPtsadj}}
}
\examples{
## Calculation of a single set of values.
calc2Node(22, 25, .50, 50)

## Using several rows of data:
ta <- c(20,22,24)
tr <- ta
vel <- rep(.15,3)
rh <- rep(50,3)

maxLength <- max(sapply(list(ta, tr, vel, rh), length))
SET <- sapply(seq(maxLength), function(x) { calcSET(ta[x], tr[x], vel[x], rh[x]) } ) 
}
\keyword{ manip }