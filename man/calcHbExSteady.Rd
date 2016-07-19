\name{calcHbExSteady}
\alias{calcHbExSteady}
\alias{HbExSteady}
\alias{HBxst}
\alias{hbxst}
\title{
Calculates Human Body Exergy Consumption Rate Using Steady State Method
}
\description{
\code{calcHbExSteady} calculates the human body exergy consumption rate  in W/m2 using steady state method based on a set of environmental variables.
}
\usage{
calcHbExSteady(ta, tr, rh, vel, clo, met, tao, rho, frad = 0.7, eps = 0.95, ic = 1.085, 
ht = 171, wt = 70, tcr = 37, tsk = 36, basMet = 58.2, warmUp = 60, cdil = 100, 
sigmatr = 0.25)
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
  \item{tao}{
a numeric value presenting outdoor air temperature in [degree C] 
}
  \item{rho}{
a numeric value presenting outdoor relative humidity [\%]
}
  \item{frad}{
a numeric value presenting the fraction of body exposed to radiation 0.7(for seating), 0.73(for standing) [-]
}
  \item{eps}{
a numeric value presenting emissivity [-]
}
  \item{ic}{
a numeric value presenting permeability of clothing: 1.084 (average permeability), 0.4 (low permeability) 
}
  \item{ht}{
a numeric value presenting body height in [cm] 
}
  \item{wt}{
a numeric value presenting body weight in [kg] 
}
  \item{tcr}{
a numeric value presenting initial value for core temperature in [degree C] 
}
  \item{tsk}{
a numeric value presenting initial value for skin temperature in [degree C] 
}
  \item{basMet}{
a numeric value presenting basal metabolic rate in [met]
}
  \item{warmUp}{
a numeric value presenting length of warm up period, i.e. number of times, loop is running for HBx calculation
}
  \item{cdil}{
a numeric value presenting value for cdil in 2-node model of Gagge
}
  \item{sigmatr}{
a numeric value presenting value for cdil in 2-node model of Gagge
}
}
\value{
Returns a data.frame with the following columns

Exergy input
  \item{xInmets }{Exergy input through metabolism}
  \item{xInmetwcs }{Label warm/ cold for exergy input through metabolism}
  \item{xInAIRwcs }{Exergy input through inhaled humid air}
  \item{xInAIRwcwcs }{Label warm/ cold for exergy input through inhaled humid air}
  \item{xInAIRwds }{Exergy input through inhaled dry air}
  \item{xInAIRwdwds }{Label wet/ dry for exergy input through inhaled dry air}
  \item{xInLUNGwcs }{Exergy input through water lung}
  \item{xInLUNGwcwcs }{Label warm/ cold for exergy input through water lung}
  \item{xInLUNGwds }{Exergy input through water lung}
  \item{xInLUNGwdwds }{Label wet/ dry for exergy input through water lung}
  \item{xInsheLLwcs }{Exergy input through water from sweat}
  \item{xInsheLLwcwcs }{Label warm/ cold for exergy input through water from sweat}
  \item{xInsheLLwds }{Exergy input through water from sweat}
  \item{xInsheLLwdwds }{Label wet/ dry for exergy input through water from sweat}
  \item{xInraDs }{Exergy input through radiation}
  \item{xInraDwcs }{Label warm/ cold for exergy input through radiation}
  \item{xIntotaLs }{total exergy input}
		
Exergy output
  \item{xoutstorecores }{Exergy stored in core}
  \item{xoutstoreshels }{Exergy stored in shell}
  \item{xoutaIRwcs }{Exergy output through exhaled humid air}
  \item{xoutaIRwcwcs }{Label warm/ cold for exergy output through exhaled humid air}
  \item{xoutaIRwds }{Exergy output through exhaled dry air}
  \item{xoutaIRwdwds }{Label wet/ dry for exergy output through exhaled dry air}
  \item{xoutswEATwcs }{Exergy output through water vapour from sweat}
  \item{xoutswEATwcwcs }{Label warm/ cold for exergy output through water vapour from sweat}
  \item{xoutswEATwds }{Exergy output through water vapour from sweat}
  \item{xoutswEATwdwds }{Label wet/ dry for exergy output through water vapour from sweat}
  \item{xoutraDs }{Exergy output through radiation}
  \item{xoutraDwcs }{Label warm/ cold for exergy output through radiation}
  \item{xoutCONVs }{Exergy output through convection}
  \item{xoutCONVwcs }{Label warm/ cold for exergy output through convection}
  \item{xouttotaLs }{total exergy output}

Exergy balance
  \item{xconss }{total exergy consumption}
  \item{xConsumption }{total exergy consumption}

Additional values
  \item{tsks }{Calculated skin temperature}
  \item{tcrs }{Calculated core temperature}
  \item{ws }{Calculated skin wettedness}
}
\references{
Schweiker, M., Kolarik, J., Dovjak, M. and Shukuya, M. Unsteady-state human-body exergy consumption rate and its relation to subjective assessment of dynamic thermal environments, Energy and Buildings , 2016, 116, 164 - 180

Shukuya, M. Calculation of human body-core and skin-layer temperatures under unsteady-state conditions-for unsteady-state human-body exergy analysis-, internal report of exergy-research group, Tech. rep., KIT/TCU, 2015.
}
\author{
This function is based on a VBA code developed by Masanori Shukuya. transformation of VBA-code and Excel procedures into R syntax by Marcel Schweiker.
}
\note{
According to Gagge's paper (1973), the value of 'cdil' may vary between 75 and 225 and 'sigma-tr' between 0.25 and 0.75. There is a note in the appendix of his paper saying two things:  1) whatever the values taken for cdil and sigma-tr, there must be no significant change in resulting thermal equilibrium. But, the values taken for cdil and sigmaTr do affect time to equilibrium. According to the analysis of schweiker et al. (2015), the values of 100 and .25 lead to the best fit of calculated and observed skin temperature. 
}
\seealso{
see also \code{\link{calcComfInd}}, \code{\link{calcHbExUnsteady}}
}
\examples{
## Calculation of human body exergy consumption rate
calcHbExSteady(22, 24, 50, .1, .8, 1.2, 5, 80)

## Calculation of multiple values
dfData <- data.frame(ta=c(20:25), tr=c(20:25))
dfResult <- calcHbExSteady(22, 24, 50, .1, .8, 1.2, 5, 80) 
for(i in 1:nrow(dfData)){
dfResult[i,] <- calcHbExSteady(dfData$ta[i], dfData$tr[i], 50, .1, .5, 1.1, 5, 80)
}


}
\keyword{ manip }
