\name{calcAvgAcc}
\alias{calcAvgAcc}
\alias{AvgAcc}
\alias{avgAcc}
\alias{avgacc}
\title{
Calculating Average Accuracy between Predicted and Actual Thermal Sensation Vote
}
\description{
\code{calcAvgAcc} calculates the average accuracy between predicted thermal sensation votes and actual obtained sensation votes
}
\usage{
calcAvgAcc(ref, pred)
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
\code{calcAvgAcc} returns a single value presenting the average accuracy between actual and predicted thermal sensation votes. 
}
\references{
Sokolova, M. and Lapalme, G. A systematic analysis of performance measures for classification tasks, Information Processing & Management, Elsevier, 2009, 45, 427-437
}
\author{
Marcel Schweiker
}
\note{
The outcome heavily depends on the distribution of actual votes, i.e. in case most of the actual votes are in the same category, e.g. 'neutral', the average accuray is very high due to the fact that for the other categories the number of TRUE negative predicted votes is high as well. 
}
\seealso{
see also \code{\link{calcTPRTSV}}, \code{\link{calcMeanBias}}
}
\examples{
## Define data
ref <- rnorm(5) # actual thermal sensation votes
ref <- cutTSV(ref)

pred <- rnorm(5) # predicted thermal sensation votes
pred <- cutTSV(pred)

calcAvgAcc(ref, pred)
}
\keyword{ manip }
