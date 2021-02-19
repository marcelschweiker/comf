#' True Positive Rate between Predicted and Actual Thermal Sensation Vote
#' @description \code{calcTPRTSV} calculates the true positive rate between 
#' predicted thermal sensation votes and actual obtained sensation votes
#' @aliases TPR
#' @aliases tpr
#' @usage calcTPRTSV(ref, pred)
#' @param ref a numeric item or vector containing categorical actual thermal 
#' sensation votes coded from -3 'cold' to +3 'hot'
#' @param pred a numeric item or vector containing categorical predicted thermal 
#' sensation votes coded from -3 'cold' to +3 'hot'
#'
#' @return \code{calcTPRTSV} returns a single value presenting the true positive 
#' rate between actual and predicted thermal sensation votes. 
#' 
#' @references Schweiker & Wagner (2015) <doi:10.1016/j.buildenv.2015.08.018>
#' 
#' @author Marcel Schweiker
#' 
#' @seealso see also \code{\link{calcMeanBias}}, \code{\link{calcAvgAcc}}
#' 
#' @examples 
#' ## Define data
#' ref <- rnorm(5) # actual thermal sensation votes
#' ref <- cutTSV(ref)
#' pred <- rnorm(5) # predicted thermal sensation votes
#' pred <- cutTSV(pred)
#' calcTPRTSV(ref, pred)
#' 
#' @keywords manip
#' @export
#'
calcTPRTSV <- function(ref, pred){
  (table(ref, pred)[1, 1] + table(ref, pred)[2, 2] + table(ref, pred)[3, 3] + table(ref, pred)[4, 4] + table(ref, pred)[5, 5] + table(ref, pred)[6, 6] + table(ref, pred)[7, 7]) / sum(table(ref, pred))
}
