\name{calcTroin}
\alias{calcTroin}
\alias{Troin}
\alias{troin}
\title{
Calculation of Radiative and Operative Temperature
}
\description{
The functions \code{calcTroin} calculates radiative and operative temperature based on air temperature, globe temperature, air velocity and metabolic rate. Globe temperature needs to be measured using a standard globe with a diameter of 0.15m and an emissivity of .95 (black coloured).
}
\usage{
calcTroin(vel, tg, ta, met)
}
\arguments{
  \item{vel}{
numeric value or vector presenting the air velocity in [m/s]
}
  \item{tg}{
numeric value or vector presenting the globe temperature in [degree C] 
}
  \item{ta}{
numeric value or vector presenting the air temperature in [degree C] 
}
  \item{met}{
numeric value or vector presenting the metabolic rate in [met]
}
}
\details{
Calculation of the radiative temperature is based on ISO 7726:2001, equation (9). Calculation of operative temperature is based on ISO 7726:2001, Appendix G.3. The adjustment of air velocity to present relative air velocity based on metabolic rate is based on ISO 7730:2005 Appendix C.2.
}
\value{
\code{calcTroin} returns a data.frame with radiative and operative temperature.
}
\references{
IsO 7726 Ergonomics of the Thermal Environment, Instruments for measuring Physical Quantities Geneva: International standard Organization, 1998

IsO 7730 Ergonomics of the thermal environment - analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005
}
\author{
Marcel Schweiker
}
\examples{
## Note. Due to random generated asv values. The values for the coefficients will not be meaningful.
## Create sample data
ta  <- 20:24      # vector with air temperature values
vel <- rep(.1,5)  # vector with air velocities
met <- rep(1.1,5) # vector with metabolic rates
tg <- 25:29       # vector with globe temperature values

calcTroin(vel, tg, ta, met)

}
\keyword{ manip }