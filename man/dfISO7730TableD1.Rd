\name{dfISO7730TableD1}
\alias{dfISO7730TableD1}
\alias{dfISO7730}
\docType{data}
\title{
Adjusted data from Table D1 of ISO 7730
}
\description{
Callibration data for SET model (used for calculation of PMVadj) from ISO 7730 Table D1
}
\usage{data(dfISO7730TableD1)}
\format{
  A data frame with 13 rows on the following 8 variables.
  \describe{
    \item{\code{ta}}{a numeric vector of air temperature [degree C]}
    \item{\code{tr}}{a numeric vector of radiant temperature [degree C]}
	\item{\code{vel}}{a numeric vector of indoor air velocity [m/s]}
    \item{\code{rh}}{a numeric vector of relative humidity [\%]}
	\item{\code{met}}{a numeric vector of metabolic rate [MET]}
    \item{\code{clo}}{a numeric vector of clothing insulation level [CLO]}
    \item{\code{pmv}}{a numeric vector of Predicted mean vote (PMV)}
    \item{\code{ppd}}{a numeric vector of Predicted percentage dissatisfied (PPD)} 
  }
}
\references{
ISO 7730 Ergonomics of the thermal environment analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005.
}
\note{
One value differs from ISO 7730 Table D1, as a comparison with the calculation done by this package and the values given by the CBE Thermal Comfort Tool (ASHRAE approved) showed a different value. The difference is:
-row 7, PMV must be .36 (not .5) and PPD 8 (not 10)
}
\examples{
data(dfISO7730TableD1)
head(dfISO7730TableD1)

PMV <- sapply(seq(nrow(dfISO7730TableD1)), function(x) { calcPMV(dfISO7730TableD1$ta[x], 
dfISO7730TableD1$tr[x], dfISO7730TableD1$vel[x], dfISO7730TableD1$rh[x], dfISO7730TableD1$clo[x], 
dfISO7730TableD1$met[x]) } ) 

PPD <- sapply(seq(nrow(dfISO7730TableD1)), function(x) { calcPPD(dfISO7730TableD1$ta[x], 
dfISO7730TableD1$tr[x], dfISO7730TableD1$vel[x], dfISO7730TableD1$rh[x], dfISO7730TableD1$clo[x], 
dfISO7730TableD1$met[x]) } ) 

plot(PMV~dfISO7730TableD1$pmv)
data.frame(pmv=dfISO7730TableD1$pmv, PMV)

plot(PPD~dfISO7730TableD1$ppd)
data.frame(ppd=dfISO7730TableD1$ppd, PPD)

}
\keyword{datasets}
