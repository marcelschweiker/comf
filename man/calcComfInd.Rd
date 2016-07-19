\name{calcComfInd}
\alias{calcComfInd}
\title{
Calculating one or more Thermal Comfort Indices using a List of Climatic Conditions
}
\description{
\code{calcComfInd} calculates one or more thermal comfort indices using a list of climatic conditions. 
}
\usage{
calcComfInd(lsCond, request = "all")
}
\arguments{
  \item{lsCond}{
a list of climatic conditions and additional variables necessary for one or more of the indices (see details below). 
}
  \item{request}{
a vector with one or more comfort indices (see details below).
}
}
\details{
The list \code{lsCond} could contain one or more of the following variables:

\tabular{ll}{
   ta    	\tab Air temperature in (degree C)  \cr
    tr    	\tab mean radiant temperature in (degree C) \cr
    vel   	\tab Air velocity in (m/s) \cr
    rh    	\tab Relative Humidity (\%) \cr
    clo   	\tab clothing (clo) \cr
    met   	\tab metabolic rate (met) \cr
    wme   	\tab External work (met) \cr
    tu    	\tab turbulence intensity (\%) \cr
    tmmo  	\tab mean monthly outdoor temperature in (degree C) \cr
    ltime 	\tab Exposure time (min) \cr
    pb    	\tab Barometric pressure (torr) \cr
    wt    	\tab weight (kg) \cr
    ht    	\tab height (cm) \cr
	trm   	\tab Running mean outdoor temperature in (degree C) \cr
	age	  	\tab age (years) \cr
	gender 	\tab gender (female = 1) \cr
	tsk   	\tab mean skin temperature in (degree C) \cr
	psych 	\tab factor related to fixed effect on perceived control \cr
	apCoeff \tab adaptive coefficient for pmv \cr
	epCoeff \tab expectancy factor for pmv \cr
	asCoeff \tab adaptive coefficient for set \cr
	esCoeff \tab expectancy factor for set \cr
	asv     \tab actual sensation vote (0 = neutral) \cr
	tao		\tab outdoor air temperature \cr
	rho		\tab outdoor relative humidity \cr
	frad 	\tab 0.7(for seating), 0.73(for standing) [-]  \cr
	eps 	\tab emissivity [-] \cr
	ic 	    \tab 1.084 (average permeability), 0.4 (low permeability)  \cr
	tcr		\tab initial values for core temp \cr
	tsk		\tab initial values for skin temperature \cr
	basMet	\tab basal metabolic rate \cr
	warmUp	\tab length of warm up period, i.e. number of times, loop is running for HBx calculation \cr
	cdil	\tab value for cdil in 2-node model of Gagge (applied in calculation of HbEx) \cr
	sigmatr \tab value for cdil in 2-node model of Gagge (applied in calculation of HbEx)
}

In case a variable is not given, but necessary for the respective index, a standard value from a list of values is used.

The vector \code{request} can contain the following elements:

\tabular{lll}{
	Element \tab Description \tab Required variables \cr
	"all" \tab Calculation of all indices described below \tab all variables \cr
	"pmv" \tab Predicted mean vote \tab  ta, tr, vel, rh, clo, met, wme \cr
	"ppd" \tab Predicted precentage dissatisfied \tab ta, tr, vel, rh, clo, met, wme \cr
	"tnhumphreys" \tab Neutral temperature according to Humphreys \tab tmmo \cr
	"tAdapt15251" \tab Adaptive comfort temperature according to EN 15251 \tab trm \cr
	"dTNZ" \tab Distance to thermoneutral zone \tab ht, wt, age, gender, clo, vel, tsk, ta \cr
	"ATHBpmv" \tab Adaptive thermal heat balance vote based on pmv \tab ta, tr, vel, rh, met, wme, psych, trm \cr
	"ATHBset" \tab Adaptive standard effective temperature \tab ta, tr, vel, rh, trm, met, wme, pb, ltime, ht, wt, psych \cr
	"ATHBpts" \tab Adaptive thermal heat balance vote based on set \tab ta, tr, vel, rh, trm, met, wme, pb, ltime, ht, wt, psych \cr
	"apmv" \tab Adaptive predicted mean vote according to Yao et al. \tab ta, tr, vel, rh, clo, met, wme, apCoeff \cr
	"ptsa" \tab Adaptive predicted thermal sensation vote according to Gao et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, asCoeff \cr
	"epmv" \tab pmv adjusted with expectancy factor based on Fanger and toftum \tab ta, tr, vel, rh, clo, met, wme, epCoeff, asv \cr
	"ptse" \tab Predicted thermal sensation vote based on set and adjusted with expectancy factor according to Gao et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, esCoeff, asv \cr
	"set" \tab standard effective temperature based on two node model by Gagge et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
	"et" \tab Effective temperature based on two node model by Gagge et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
	"tsens" \tab Predicted thermal sensation \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
	"disc" \tab Predicted discomfort \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
	"ps" \tab Predicted percentage satisfied with the level of air movement \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
	"pd" \tab Predicted percentage dissatisfied due to draft \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, tu \cr
	"pts" \tab Predicted thermal sensation vote based on set \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
	"HBxst" \tab Human body exergy consumPtion rate using steady state method \tab ta, tr, vel, rh, clo, met, tao, rho, frad, eps, ic, ht, wt, tcr, tsk, basMet, warmUp, cdil, sigmatr
}
}
\value{
\code{calcComfInd} returns one or more rows with the comfort indices listed as \code{request}. For details see details above. 
}
\references{
For references see individual functions.
}
\author{
Sophia Mueller and Marcel Schweiker
}
\note{
In case one of the variables is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
}
\seealso{
see also \code{\link{calcPMVPPD}}, \code{\link{calc2Node}}, \code{\link{calcHbExSteady}}, \code{\link{calcATHB}}, \code{\link{calcdTNZ}}, \code{\link{calcpmvadj}}, \code{\link{calcPtsadj}}, \code{\link{calctadapt}} 
}
\examples{
## Creating list with all values
lsstrd <- createCond()

## Requesting all comfort indices
calcComfInd(lsstrd, request="all")

## Requesting a single index
calcComfInd(lsstrd, request="pmv")

## Requesting multiple indices
calcComfInd(lsstrd, request=c("pmv", "ptse"))

}
\keyword{ manip }
