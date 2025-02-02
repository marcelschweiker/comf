#' @title Average Accuracy between Predicted and Actual Thermal Sensation Vote
#' @description \code{calcAvgAcc} calculates the average accuracy between predicted thermal sensation votes and actual obtained sensation votes
#' @aliases calcavgacc
#' @aliases AvgAcc
#' @aliases avgacc
#' @usage calcAvgAcc(ref, pred)
#' @usage calcavgacc(ref, pred)
#' @usage AvgAcc(ref, pred)
#' @usage avgacc(ref, pred)
#' @param ref a numeric item or vector containing categorical actual thermal sensation votes coded from -3 'cold' to +3 'hot'
#' @param pred a numeric item or vector containing categorical predicted thermal sensation votes coded from -3 'cold' to +3 'hot'
#' @returns \code{calcAvgAcc} returns a single value presenting the average accuracy between actual and predicted thermal sensation votes.
#' @examples ## Define data
#' ref <- rnorm(5) # actual thermal sensation votes
#' ref <- cutTSV(ref)
#'
#' pred <- rnorm(5) # predicted thermal sensation votes
#' pred <- cutTSV(pred)
#'
#' calcAvgAcc(ref, pred)
#' @author  Marcel Schweiker. Further contribution by Shoaib Sarwar.
#' @references  Sokolova and Lapalme (2009) <doi:10.1016/j.ipm.2009.03.002>
#' @seealso \code{\link{calcTPRTSV}}, \code{\link{calcMeanBias}}
#' @note The outcome heavily depends on the distribution of actual votes, i.e. in case most of the actual votes are in the same category, e.g. 'neutral', the average accuray is very high due to the fact that for the other categories the number of TRUE negative predicted votes is high as well.
#' @export

calcAvgAcc <- function(ref, pred) {
  classes <- sort(unique(c(unique(as.numeric(as.character(ref))), unique(as.numeric(as.character(pred))))))

  tp <- tn <- fp <- fn <- NA
  for (i in 1:length(classes)) { # calculation tp, tn, fn, fp for each class
    tp[i] <- length(which(ref == classes[i] & pred == classes[i]))
    tn[i] <- length(which(ref != classes[i] & pred != classes[i]))
    fn[i] <- length(which(ref == classes[i] & pred != classes[i]))
    fp[i] <- length(which(ref != classes[i] & pred == classes[i]))
  }

  tptn <- tp + tn
  n <- tp + tn + fn + fp
  acci <- tptn / n # vector with accuracy of each class

  acc <- sum(acci) / length(classes)
  acc
}
calcavgacc <- calcAvgAcc
AvgAcc <- calcAvgAcc
avgacc <- calcAvgAcc
