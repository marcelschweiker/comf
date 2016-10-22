\name{dfISO7730AppE}
\alias{dfISO7730AppE}
\docType{data}
\title{
Adjusted data from Appendix E of ISO 7730
}
\description{
Callibration data for PMV/PPD model from ISO 7730 Appendix E
}
\usage{data(dfISO7730AppE)}
\format{
  A data frame with 2963 rows on the following 6 variables.
  \describe{
	\item{\code{top}}{a numeric vector of operative temperature [degree C]}
	\item{\code{vel}}{a numeric vector of indoor air velocity [m/s]}
	\item{\code{rh}}{a numeric vector of relative humidity [\%]}
	\item{\code{met}}{a numeric vector of metabolic rate [MET]}
	\item{\code{clo}}{a numeric vector of clothing insulation level [CLO]}
	\item{\code{pmv}}{a numeric vector of Predicted mean vote (PMV)} 
	}
}
\references{
ISO 7730 Ergonomics of the thermal environment analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005.

Fanger, P. O. Thermal Comfort Analysis and Applications in Environmental Engineering McGraw-Hill, New York, 1970.

}
\note{
Several values differ from ISO 7730 Appendix E, as a comparison with the calculation done by this package, the values given by the CBE Thermal Comfort Tool (ASHRAE approved) and the values presented in the original source by Fanger showed a different value. 
}
\examples{
data(dfISO7730AppE)
head(dfISO7730AppE)

PMV <- sapply(seq(nrow(dfISO7730AppE)), function(x) { calcPMV(dfISO7730AppE$top[x], 
dfISO7730AppE$top[x], dfISO7730AppE$vel[x], dfISO7730AppE$rh[x], dfISO7730AppE$clo[x], 
dfISO7730AppE$met[x]) } ) 

plot(PMV~dfISO7730AppE$pmv)

}
\keyword{datasets}
