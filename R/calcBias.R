#' @title Bias between Predicted and Actual Thermal Sensation Vote
#' @description \code{calcMeanBias} calculates the mean bias and its standard deviation and standard error between predicted thermal sensation votes and actual obtained sensation votes
#' @aliases calcbias
#' @aliases calcMeanBias
#' @aliases MeanBias
#' @aliases meanBias
#' @aliases meanbias
#' @aliases bias
#' @aliases calcSdBias
#' @aliases calcSeBias
#' @usage calcBias(ref, pred)
#' @usage calcbias(ref, pred)
#' @usage calcMeanBias(ref, pred)
#' @usage MeanBias(ref, pred)
#' @usage meanBias(ref, pred)
#' @usage meanbias(ref, pred)
#' @usage bias(ref, pred)
#' @usage calcSdBias(ref, pred)
#' @usage calcSeBias(ref, pred)
#' @param ref a numeric item or vector containing categorical actual thermal sensation votes coded from -3 'cold' to +3 'hot'
#' @param pred a numeric item or vector containing categorical predicted thermal sensation votes coded from -3 'cold' to +3 'hot'
#' @returns \code{calcMeanBias} returns a dataframe with the following items:
#' \item{meanBias}{single value presenting the mean bias between actual and predicted thermal sensation votes}
#' \item{sdBias}{single value presenting the standard deviation of the mean bias}
#' \item{seBias}{single value presenting the standard error of the mean bias}
#' @examples ## Define data
#' ref <- rnorm(5) # actual thermal sensation votes
#'
#' pred <- rnorm(5) # predicted thermal sensation votes
#'
#' calcBias(ref, pred)
#' @author  Marcel Schweiker. Further contribution by Shoaib Sarwar.
#' @references  Humphreys & Nicol (2002) <doi:10.1016/S0378-7788(02)00018-X>
#' @references Schweiker & Wagner (2016) Exploring potentials and limitations of the adaptive thermal heat balance framework Proceedings of 9th Windsor Conference: Making Comfort Relevant Cumberland Lodge, Windsor, UK, 2016.
#' @seealso \code{\link{calcTPRTSV}}, \code{\link{calcAvgAcc}}
#' @export


calcBias <- function(ref, pred) {
  ref <- ifelse(rep(is.factor(ref), length(ref)), as.numeric(as.character(ref)), ref)
  pred <- ifelse(rep(is.factor(pred), length(pred)), as.numeric(as.character(pred)), pred)
  bias <- pred - ref
  meanBias <- mean(bias, na.rm = T)
  sdBias <- sd(bias)
  seBias <- sdBias / sqrt(length(ref))

  data.frame(meanBias, sdBias, seBias)
}
calcbias <- calcBias
calcMeanBias <- calcBias
MeanBias <- calcBias
meanBias <- calcBias
meanbias <- calcBias
bias <- calcBias
calcSdBias <- calcBias
calcSeBias <- calcBias
