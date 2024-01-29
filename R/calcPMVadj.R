#' @title Predicted Mean Votes adjusted for elevated air speed
#' @description Function to calculate Predicted Mean Votes (PMV) adjusted for cooling effect of elevated air speed.
#' @aliases PMVadj
#' @aliases pmvadj
#' @usage calcPMVadj(ta, tr, vel, rh, clo, met, wme = 0)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @details Calculated through function Two node model(\code{calc2Node}
#' @returns \code{calcpmvadj} returns the predicted mean vote adjusted for the cooling effect of elevated air speed.
#' @examples calcPMVadj(25,25,0.3,50,0.5,1)
#' @author Code implemented in to R by Marcel Schweiker. Further contribution by Sophia Mueller and Shoaib Sarwar.
#' @references pmvadj is based on ASHRAE standard 55-2013. Thermal environmental conditions for human occupancy. American society of heating, Refrigerating and Air-Conditioning Engineering, Atlanta, USA, 2013.
#' @export


calcPMVadj <- function(ta, tr, vel, rh, clo, met, wme = 0){

  f <- function(x){calc2Node(ta, tr, vel, rh, clo, met, wme, NULL, pb = 760, ltime = 60, ht = 171, wt = 70, tu = 40, obj = "pmvadj")[2] - calc2Node(ta + x, tr + x, .1, rh, clo, met, wme, NULL, pb = 760, ltime = 60, ht = 171, wt = 70, tu = 40, obj = "pmvadj")[2]}
  ce <- bisect(f, -15, 1)$x

  pmvadj <- calcPMVPPD(ta + ce, tr + ce, .1, rh, clo, met, wme)[1]
  names(pmvadj) <- "pmvadj"
  data.frame(pmvadj=pmvadj)
}
