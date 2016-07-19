\name{calcTPRTSV}
\alias{calcTPRTSV}
\alias{TPR}
\alias{tpr}
\title{
Calculating the True Positive Rate between Predicted and Actual Thermal Sensation Vote
}
\description{
\code{calcTPRTSV} calculates the true positive rate between predicted thermal sensation votes and actual obtained sensation votes
}
\usage{
calcTPRTSV(ref, pred)
}
\arguments{
  \item{ref}{
a numeric item or vector containing categorical actual thermal sensation votes coded from -3 'cold' to +3 'hot'
}
  \item{pred}{
a numeric item or vector containing categorical predicted thermal sensation votes coded from -3 'cold' to +3 'hot'
}
}
\value{
\code{calcTPRTSV} returns a single value presenting the true positive rate between actual and predicted thermal sensation votes. 
}
\references{
Schweiker, M. and Wagner, A. A framework for an adaptive thermal heat balance model (ATHB) Building and Environment, 2015, 94, 252-262
}
\author{
Marcel Schweiker
}
\seealso{
see also \code{\link{calcMeanBias}}, \code{\link{calcAvgAcc}}
}
\examples{
## Define data
ref <- rnorm(5) # actual thermal sensation votes
ref <- cutTSV(ref)

pred <- rnorm(5) # predicted thermal sensation votes
pred <- cutTSV(pred)

calcTPRTSV(ref, pred)
}
\keyword{ manip }
