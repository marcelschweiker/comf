\name{comf-package}
\alias{comf-package}
\alias{comf}
\docType{package}
\title{
Calculation and Evaluation of Common and Less Common Comfort Indices 
}
\description{
This package contains several functions to calculate and evaluate a series of comfort indices.
}
\details{
\tabular{ll}{
Package: \tab comf\cr
Type: \tab Package\cr
Version: \tab 0.1.8\cr
Date: \tab 2018-01-06\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
To create input parameters, the function \code{createCond}, which creates a list of input parameters may be helpful.
The main function of this packages is \code{calcComfInd}, which returns the desired comfort parameters. However, each index can be computed using its own function, e.g. to calculate only PMV the function \code{calcPMV} can be used.

The comfort indices calculated within this package are as follows. To get further information, go to the help page, which can be accessed using the index below e.g. ?pmv:
\tabular{ll}{
	Index \tab Description  \cr
	pmv \tab Predicted mean vote (PMV) \cr
	ppd \tab Predicted precentage dissatisfied (PPD) \cr
	tnHumphreysNV \tab Neutral temperature in naturally ventilated buildings according to Humphreys 1978 \cr
	tnHumphreysAC \tab Neutral temperature in climate-controlled buildings according to Humphreys 1978 \cr
	tnAuliciems \tab Neutral temperature according to Auliciems 1981 \cr
	tAdapt15251 \tab Adaptive comfort temperature according to EN 15251 \cr
	dTNZ \tab Distance to thermoneutral zone \cr
	ATHBpmv \tab Adaptive thermal heat balance vote based on pmv \cr
	ATHBset \tab Adaptive standard effective temperature \cr
	ATHBpts \tab Adaptive thermal heat balance vote based on set \cr
	apmv \tab Adaptive predicted mean vote according to Yao et al. \cr
	ptsa \tab Adaptive predicted thermal sensation vote according to Gao et al. \cr
	epmv \tab PMV adjusted with expectancy factor based on Fanger and toftum \cr
	ptse \tab Predicted thermal sensation vote based on set and adjusted with expectancy factor according to Gao et al. \cr
	set \tab standard effective temperature based on two node model by Gagge et al. \cr
	et \tab Effective temperature based on two node model by Gagge et al. \cr
	tsens \tab Predicted thermal sensation \cr
	disc \tab Predicted discomfort \cr
	ps \tab Predicted percentage satisfied with the level of air movement \cr
	pd \tab Predicted percentage dissatisfied due to draft \cr
	pts \tab Predicted thermal sensation vote based on set \cr
	HBxst \tab Human body exergy consumption rate using steady state method \cr
	PHS \tab Predicted heat strain
}

The performance criteria included in this package are presented below. Again you can get further information on the corresponding help pages:
\tabular{ll}{
	Index \tab Description  \cr
	meanBias \tab Mean bias between predicted and observed thermal sensation vote \cr
	TPR \tab True positive rate \cr
	avgAcc \tab Average accuracy of predicted thermal sensation vote
}
}
\author{
Marcel Schweiker in cooperation with Sophia Mueller

Contact: mschweiker@ukaachen.de

}
\references{
See references in function descriptions.
}
\keyword{ package }
\seealso{
see also \code{\link{createCond}}, \code{\link{calcComfInd}}
}
