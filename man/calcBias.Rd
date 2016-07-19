\name{calcBias}
\alias{calcBias}
\alias{calcMeanBias}
\alias{MeanBias}
\alias{meanBias}
\alias{bias}
\alias{calcSdBias}
\alias{calcSeBias}
\title{
Calculating the Bias between Predicted and Actual Thermal Sensation Vote
}
\description{
\code{calcMeanBias} calculates the mean bias and its standard deviation and standard error between predicted thermal sensation votes and actual obtained sensation votes
}
\usage{
calcBias(ref, pred)
}
\arguments{
  \item{ref}{
a numeric item or vector containing categorical or continuous actual thermal sensation votes coded from -3 'cold' to +3 'hot'
}
  \item{pred}{
a numeric item or vector containing categorical or continuous predicted thermal sensation votes coded from -3 'cold' to +3 'hot'
}
}
\value{
\code{calcMeanBias} returns a dataframe with the following items:
\item{meanBias}{single value presenting the mean bias between actual and predicted thermal sensation votes}
\item{sdBias}{single value presenting the standard deviation of the mean bias}
\item{seBias}{single value presenting the standard error of the mean bias}
 
}
\references{
Humphreys, M. A. and Nicol, J. F. The validity of ISO-PMV for predicting comfort votes in every-day thermal environments, Energy and Buildings, 2002, 34, 667-684

Schweiker, M. and Wagner, A. Exploring potentials and limitations of the adaptive thermal heat balance framework Proceedings of 9th Windsor Conference: Making Comfort Relevant Cumberland Lodge, Windsor, UK, 2016
}
\author{
Marcel Schweiker
}
\seealso{
see also \code{\link{calcTPRTSV}}, \code{\link{calcAvgAcc}}
}
\examples{
## Define data
ref <- rnorm(5) # actual thermal sensation votes

pred <- rnorm(5) # predicted thermal sensation votes

calcBias(ref, pred)
}
\keyword{ manip }
