\name{dfISO7933AppF}
\alias{dfISO7933AppF}
\docType{data}
\title{
Adjusted data from Appendix F of ISO 7933
}
\description{
Calibration data for predicted heat strain model from ISO 7933 Appendix F
}
\usage{data(dfISO7933AppF)}
\format{
  A data frame with 10 rows on the following 11 variables and 5 predicted result values.
  \describe{
	\item{\code{accl}}{a numeric vector}
	\item{\code{posture}}{a numeric vector of posture, 1 = sitting, 2 = standing, 3 = crouching []}
	\item{\code{Ta}}{a numeric vector o air temperature [degree C]}
	\item{\code{Pa}}{a numeric vector of partial water vapour pressure [kPa]}
	\item{\code{Tr}}{a numeric vector of mean radiant temperature [degree C]}
	\item{\code{Va}}{a numeric vector of air velocity [m/s]}
	\item{\code{Met}}{a numeric vector of metabolic rate [W/(m*m)]}
	\item{\code{Icl}}{a numeric vector of static thermal insulation [clo]}
	\item{\code{THETA}}{a numeric vector of angle between walking direction and wind direction, 0-360 [degree]} 
	\item{\code{Walksp}}{a numeric vector of walking speed [m/s]} 
	\item{\code{Duration}}{a numeric vector of the duration of the work sequence, usually 480 [min]}
	\item{\code{Tre}}{a numeric vector of predicted rectal temperature [degree C]}
	\item{\code{SWtotg}}{a numeric vector of predicted total water loss [g]}
	\item{\code{Dlimtre}}{a numeric vector of predicted time after which maximum rectal temperatur is reached [min]}
	\item{\code{Dlimloss50}}{a numeric vector of maximum duration to limit water loss to 7.5\% of body mass with an average person [min]}
	\item{\code{Dlimloss95}}{a numeric vector of maximum duration to limit water loss to 5.0\% of body mass with 95\% of population [min]}
	}
}

\references{
ISO 7933 Ergonomics of the thermal environment - Analytical determination and interpretation of heat stress using calculation of the predicted heat strain (ISO 7933:2004)

}
\note{
Additionally to the 5 presented predicted values in ISO 7933 some more intermediate results are exported to the dataframe. 
}
\examples{
data(dfISO7933AppF)
head(dfISO7933AppF)

dfHs <- sapply(seq(nrow(dfISO7933AppF)), function(x) { calcIso7933(dfISO7933AppF$accl[x], 
dfISO7933AppF$posture[x], dfISO7933AppF$Ta[x], dfISO7933AppF$Pa[x], dfISO7933AppF$Tr[x],
dfISO7933AppF$Va[x], dfISO7933AppF$Met[x], dfISO7933AppF$Icl[x], dfISO7933AppF$THETA[x], 
dfISO7933AppF$Walksp[x], dfISO7933AppF$Duration[x]) } ) 

}
\keyword{datasets}