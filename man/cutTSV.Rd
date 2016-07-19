\name{cutTSV}
\alias{cutTSV}
\title{
Categorizing Thermal Sensation Votes
}
\description{
\code{cutTSV} converts continuous thermal sensation votes to categorical ones.
}
\usage{
cutTSV(pred)
}
\arguments{
  \item{pred}{
a numeric item or vector containing continuous thermal sensation votes coded from -3 'cold' to +3 'hot'
}
}
\details{
Categorization is realized with intervals closed on the right, e.g. setting all values lower and equal then -2.5 to a value of -3, higher than -2.5 and lower or equal -1.5 to -2, and so on.
}
\value{
\code{cutTSV} returns an item or a vector with categorical thermal sensation votes.
}
\author{
Marcel Schweiker
}
\examples{
## define example data
pred <- rnorm(5)

## bin values
cutTSV(pred)
}
\keyword{ manip }
