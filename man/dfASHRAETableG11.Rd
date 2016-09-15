\name{dfASHRAETableG11}
\alias{dfASHRAETableG11}
\alias{dfASHRAE}
\docType{data}
\title{
Data from Table G1-1 of ASHRAE 55-2013
}
\description{
Callibration data for SET model (used for calculation of PMVadj) from ASHRAE 55-2013 Table G1-1
}
\usage{data(dfASHRAETableG11)}
\format{
  A data frame with 22 rows on the following 11 variables.
  \describe{
    \item{\code{ta}}{a numeric vector of air temperature [degree C]}
	\item{\code{taF}}{a numeric vector of air temperature [degree F]}
    \item{\code{tr}}{a numeric vector of radiant temperature [degree C]}
	\item{\code{trF}}{a numeric vector of radiant temperature [degree F]}
	\item{\code{vel}}{a numeric vector of indoor air velocity [m/s]}
	\item{\code{velFPM}}{a numeric vector of indoor air velocity [fpm]}
    \item{\code{rh}}{a numeric vector of relative humidity [\%]}
	\item{\code{met}}{a numeric vector of metabolic rate [MET]}
    \item{\code{clo}}{a numeric vector of clothing insulation level [CLO]}
    \item{\code{set}}{a numeric vector of standard effective temperature (SET) [degree C]}
    \item{\code{setF}}{a numeric vector of standard effective temperature (SET) [degree F]}   
  }
}
\references{
ASHRAE standard 55-2013. Thermal environmental conditions for human occupancy. American society of heating, Refrigerating and Air-Conditioning Engineering, Atlanta, Usa, 2013
}
\note{
Note that rows 21 and 22 can only be compared to the function \code{calcSET} in case \code{obj="pmvadj"} is chosen. The value of \code{obj} defines whether the function will use the version presented in ASHRAE 55-2013 for adjustment of PMV (\code{obj = "pmvadj"}), or the original code by Gagge to calculate the standard effective temperature (SET) (\code{obj = "set"}). The reason is, that in the version presented in ASHRAE 55-2013, the lines of code related to self-generated convection are deleted. Therefore, a difference can be seen at higher values of met.
}
\examples{
data(dfASHRAETableG11)
head(dfASHRAETableG11)

# using option obj="pmvadj" in order to compare with ASHRAE Table G1-1
SET <- sapply(seq(nrow(dfASHRAETableG11)), function(x) { calcSET(dfASHRAETableG11$ta[x], 
dfASHRAETableG11$tr[x], dfASHRAETableG11$vel[x], dfASHRAETableG11$rh[x], dfASHRAETableG11$clo[x], 
dfASHRAETableG11$met[x], obj="pmvadj") } ) 
plot(SET~dfASHRAETableG11$set)
data.frame(set=dfASHRAETableG11$set, SET)

# using option obj="set" (standard) in order to calculate values for original SET values
SET2 <- sapply(seq(nrow(dfASHRAETableG11)), function(x) { calcSET(dfASHRAETableG11$ta[x], 
dfASHRAETableG11$tr[x], dfASHRAETableG11$vel[x], dfASHRAETableG11$rh[x], dfASHRAETableG11$clo[x], 
dfASHRAETableG11$met[x], ) } ) 
plot(SET2~dfASHRAETableG11$set)
data.frame(set=dfASHRAETableG11$set, SET2)

}
\keyword{datasets}
