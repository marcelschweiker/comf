#' @title Calculate PMV and PPD
#' @description Function to calculate Predicted Mean Vote (PMV) and Predicted Percentage of Dissatisfied (PPD).
#' @aliases pmvppd
#' @usage calcPMVPPD(ta, tr, vel, rh, clo=.5, met=1, wme=0, basMet=58.15)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @param basMet a numeric value presenting basal metabolic rate [w/m2]
#' @details The PMV is an index that predicts the mean value of the thermal sensation of a large group of people on a sensation scale expressed from (-3) to (+3) corresponding to the categories cold, cool, slightly cool, neutral, slightly warm, warm and hot. The PPD is an index that establishes a quantitative prediction of the percentage of thermally dissatisfied people determined from PMV. 
#' @details Note that the adjustments in the value for basMet need to be made with great cautiousness as the PMV calculation is an empirical model and might not be valid for other values of basMet than the one commonly used.
#' @returns PMV - Predicted Mean Vote
#' @returns PPD - Predicted Percentage of Dissatisfied occupants in [\%]
#' @examples calcPMVPPD(25,25,0.3,50,0.5,1)
#' @author Code implemented in to R by Marcel Schweiker. Further contribution by Sophia Mueller and Shoaib Sarwar.
#' @seealso see also \code{\link{calcComfInd}}
#' @references Fanger, P. O. Thermal Comfort Analysis and Applications in Environmental Engineering McGraw-Hill, New York, 1970.
#' @references ISO 7730 Ergonomics of the thermal environment analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005.
#' @export




calcPMVPPD <- function(ta, tr, vel, rh, clo=.5, met=1, wme=0, basMet=58.15){

  m   <- met * basMet
  w   <- wme * basMet
  mw  <- m - w
  icl <- .155 * clo
  pa  <- rh * 10 * exp(16.6536 - (4030.183 / (ta + 235)))

  # Compute the corresponding fcl value
  if (icl <= .078){
    fcl <- 1 + 1.29 * icl
  } else {
    fcl <- 1.05 + .645 * icl
  }

  fcic <- icl * fcl
  p2   <- fcic * 3.96
  p3   <- fcic * 100
  tra  <- tr + 273
  taa  <- ta + 273
  p1   <- fcic * taa
  p4   <- 308.7 - .028 * mw + p2 * (tra / 100) ^ 4

  # First guess for surface temperature
  tclA <- taa + (35.5-ta) / (3.5 * (6.45 * icl + .1))
  xn   <- tclA / 100
  xf   <- xn
  hcf  <- 12.1 * (vel) ^ .5
  noi  <- 0
  eps  <- .00015


  # Compute surface temperature of clothing by iterations
  while (noi < 150){
    xf  <- (xf + xn) / 2
    hcn <- 2.38 * abs(100 * xf - taa) ^ .25
    if (hcf > hcn){
      hc <- hcf
    } else {
      hc <- hcn
    }
    xn  <- (p4 + p1 * hc - p2 * xf ^ 4) / (100 + p3 * hc)
    noi <- noi + 1
    if(noi > 1 & abs(xn - xf) <= eps){break}
  }
  tcl <- 100 * xn - 273

  # Compute pmv

  pm1 <- 3.96 * fcl * (xn ^ 4 - (tra / 100) ^ 4)
  pm2 <- fcl * hc * (tcl - ta)
  pm3 <- .303 * exp(-.036 * m) + .028
  if (mw > basMet){
    pm4 <- .42 * (mw - basMet)
  } else {
    pm4 <- 0
  }
  pm5 <- 3.05 * .001 * (5733 - 6.99 * mw - pa)
  pm6 <- 1.7 * .00001 * m * (5867 - pa) + .0014 * m * (34 - ta)
  pmv <- pm3 * (mw - pm5 - pm4 - pm6 - pm1 - pm2)

  ppd <- 100 - 95 * exp(-.03353 * pmv ^ 4 - .2179 * pmv ^ 2)
  data.frame(pmv, ppd)
}
