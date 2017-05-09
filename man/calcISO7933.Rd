\name{calcIso7933}
\alias{calcIso7933}
\alias{calcISO7933}
\alias{calcTre}
\alias{calcSWtotg}
\alias{calcDlimtre}
\alias{calcDlimloss50}
\alias{calcDlimloss95}
\title{
Calculating Heat Strain Indices based on ISO 7933
}
\description{
\code{calcIso7933} calculates Tre, SWtotg, Dlimtre, Dlimloss50 and Dlimloss95 based on ISO 7933.  It additionally provides intermediate results from the calculation: Cres, Eres, Ep, SWp, Texp, Tskeq, Tsk, wp
}
\usage{
calcIso7933(accl, posture, Ta, Pa, Tr, Va, Met, Icl, THETA, Walksp, Duration, 
weight, height, DRINK, Adu, spHeat, SWp, Tre, Tcr, Tsk, Tcreq, Work, imst, Ap, 
Fr, defspeed, defdir, HR, pb)
}
\arguments{
  \item{accl}{
a numeric value presenting state of acclimation [100 if acclimatised subject, 0 otherwise]
}
  \item{posture}{
a numeric value presenting posture of person [sitting=1, standing=2, crouching=3] 
}
  \item{Ta}{
a numeric value presenting air temperature in [degrees celsius] 
}
  \item{Pa}{
a numeric value presenting partial water vapour pressure [kPa]
}
  \item{Tr}{
a numeric value presenting mean radiant temperature in [degrees celsius] 
}
  \item{Va}{
a numeric value presenting air velocity in [m/s] 
}
  \item{Met}{
a numeric value presenting metabolic rate in [W/(m*m)]
}
  \item{Icl}{
a numeric value presenting static thermal insulation of clothing [clo]
}
  \item{THETA}{
a numeric value presenting angle between walking direction and wind direction in [degrees] 
}
  \item{Walksp}{
a numeric value presenting walking speed in [m/s] 
}
  \item{Duration}{
a numeric value presenting the duration of the work sequence in [min] 
}
  \item{weight}{
a numeric value presenting the body mass in [kg] 
}
  \item{height}{
a numeric value presenting the body height in [m] 
}
  \item{DRINK}{
a numeric value presenting if workers can drink as they want [1 if they can drink without restriction, 0 if restricted]
}
  \item{Adu}{
a numeric value presenting body surface area according to Du Bois [m*m]
}
  \item{spHeat}{
a numeric value presenting specific body heat [(W/(m*m))/K]
}
  \item{SWp}{
a numeric value presenting predicted sweat rate [W/(m*m)]
}
  \item{Tre}{
a numeric value presenting rectal temperature [degrees celsius]
}
  \item{Tcr}{
a numeric value presenting temperature of body core [degrees celsius]
}
  \item{Tsk}{
a numeric value presenting skin temperature at start [degrees celsius]
}
  \item{Tcreq}{
a numeric value presenting temperature of body core dependent on energy metabolism [degrees celsius]
}
  \item{Work}{
a numeric value presenting effective mechanical power [W/(m*m)]
}
  \item{imst}{
a numeric value presenting static moisture permeability index [-]
}
  \item{Ap}{
a numeric value presenting fraction of the body surface covered by the reflective clothing [-]
}
  \item{Fr}{
a numeric value presenting emissivity of the reflective clothing [-]
}
  \item{defspeed}{
a numeric value presenting if walking speed entered [1 if walking speed entered, 0 otherwise]
}
  \item{defdir}{
a numeric value presenting if walking direction entered [1 if walking direction entered, 0 otherwise]
}
  \item{HR}{
a numeric value presenting humidity ratio [g/kg]
}
  \item{pb}{
a numeric value presenting normal barometric pressure in [Pa]
}
}
\details{
All variables must have the same length 1.
}
\value{
\code{calcISO7933} returns a data.frame with the following items:
\item{Tre}{final rectal temperature [degrees Celsius]}
\item{SWtotg}{total water loss [g]}
\item{Dlimtre}{time when limit for rectal temperature is reached [min]}
\item{Dlimloss50}{time when limit for water loss Dmax50 (7.5 percent of body mass of an average person) is reached [min]}
\item{Dlimloss95}{time when limit for water loss Dmax95 (5 percent of body mass of 95 percent of the working people) is reached [min]}
\item{Cres}{convective heat flow at respiration [W/(m*m)]}
\item{Eres}{evaporative heat flow at respiration [W/(m*m)]}
\item{Ep}{predicted evaporative heat flow [W/(m*m)]}
\item{SWp}{predicted sweating rate [W/(m*m)]}
\item{Texp}{temperature of the exhaled air [degrees Celsius]}
\item{Tskeq}{skin Temperature in equilibrium [degrees Celsius]}
\item{Tsk}{skin Temperature at the minute [degrees Celsius]}
\item{wp}{predicted skin wettedness [-]}
}

\references{
Ergonomics of the thermal environment - Analytical determination and interpretation of heat stress using calculation of the predicted heat strain (ISO 7933:2004)
Malchaire J., Piette A., Kampmann B., Mehnert P., Gebhardt H. J., Havenith G., Den Hartog E., Holmer I., Parsons K., Alfano G., Griefahn B. (2000), Development and validation of the predicted heat strain model, The Annals of Occupational Hygiene The Annals of Occupational Hygene, 45, pp 123-135
Malchaire J., Kampmann B., Havenith G., Mehnert P., Gebhardt H. J. (2000), Criteria for estimating acceptable exposure times in hot work environment, a review, International Archives of Occupational and Environmental Health, 73(4), pp. 215-220.
}
\author{
The code for \code{calcISO7933} is based on the code in BASIC presented in Addendum E of EN ISO 7933. The translation into R-language conducted by Michael Kleber.
}
\note{
In case one of the variables is not given, a standard value according to ISO 7933 will be taken.
}

\examples{
## Calculation of a single set of values.
calcIso7933(accl = 100, posture = 2, Ta = 35, Pa = 4, Tr = 35, Va = 0.3, Met = 150, 
Icl = 0.5, THETA = 0, Walksp = 0, Duration = 480)

calcIso7933(100,2,35,4,35,0.3,150,0.5,0,0,480)
## Using several rows of data:
accl <- 100
posture <- 2
Ta <- c(40,35)
Pa <- c(2.5,4)
Tr <- c(40,35)
Va <- 0.3
Met <- 150
Icl <- 0.5
THETA <- 0
Walksp <- 0
Duration <- 480
maxLength <- max(sapply(list(accl, posture, Ta, Pa, Tr, Va, Met, Icl, THETA, Walksp, 
Duration), length))

PHI <- sapply(seq(maxLength), function(x) {calcIso7933(accl, posture, Ta[x], Pa[x], 
Tr[x], Va, Met, Icl, THETA, Walksp, Duration) } ) 
}
\keyword{ manip }