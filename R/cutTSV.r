#' Categorizing Thermal Sensation Votes
#' @description \code{cutTSV} converts continuous thermal sensation votes to
#' categorical ones.
#' @param pred a numeric item or vector containing continuous thermal sensation
#' votes coded from -3 'cold' to +3 'hot'
#'
#' @return \code{cutTSV} returns an item or a vector with categorical thermal
#' sensation votes.
#' @usage cutTSV(pred)
#' @details Categorization is realized with intervals closed on the right, e.g.
#' setting all values lower and equal then -2.5 to a value of -3, higher than
#' -2.5 and lower or equal -1.5 to -2, and so on.
#'
#' @author Marcel Schweiker
#' @examples ## define example data
#' pred <- rnorm(5)
#' ## bin values
#' cutTSV(pred)
#' @keywords manip
#' @export
#'
cutTSV <- function(pred) {
  cut(pred, breaks = c(-300, -2.5, -1.5, -.5, .5, 1.5, 2.5, 300), labels = c("-3", "-2", "-1", "0", "1", "2", "3"))
}
