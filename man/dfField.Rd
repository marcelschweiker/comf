\name{dfField}
\alias{dfField}
\docType{data}
\title{
Data from Field Study Campaign
}
\description{
Randomly sampled data from a field study campaign with data from 156 samples. For further description, see the reference given.
}
\usage{data(dfField)}
\format{
  A data frame with 156 observations on the following 9 variables.
  \describe{
    \item{\code{ta}}{a numeric vector of air temperature [degree C]}
    \item{\code{tr}}{a numeric vector of radiant temperature [degree C] - same as ta}
    \item{\code{rh}}{a numeric vector of relative humidity [\%]}
    \item{\code{trm}}{a numeric vector of running mean outdoor temperature [degree C]}
    \item{\code{clo}}{a numeric vector of clothing insulation level [CLO]}
    \item{\code{tao}}{a numeric vector of outdoor air temperature [degree C]}
    \item{\code{vel}}{a numeric vector of indoor air velocity [m/s]}
    \item{\code{met}}{a numeric vector of metabolic rate [MET]}
    \item{\code{asv}}{a numeric vector of actual thermal sensation vote on ASHRAE scale [ ]}
  }
}
\references{
Schweiker, M. and Wagner, A. Exploring potentials and limitations of the adaptive thermal heat balance framework. Proceedings of 9th Windsor Conference: Making Comfort Relevant Cumberland Lodge, Windsor, UK, 2016.
}
\examples{
data(dfField)
head(dfField)
}
\keyword{datasets}
