#' @title Required recovery time
#' @description Function to calculate Required recovery time, RT in hours.
#' @aliases  calcrt
#' @aliases  RT
#' @aliases  rt
#' @usage calcRT(M,W,ta,tr,p,w,v,rh,clo)
#' @param M a numeric value presenting metabolic energy production (58 to 400 W/m2) in [W/m2]
#' @param W a numeric value presenting Rate of mechanical work, (normally 0) in [W/m2]
#' @param ta a numeric value presenting ambiant air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param p a numeric value presenting air permeability (low < 5, medium 50, high > 100 l/m2s) in [l/m2s]
#' @param w a numeric value presenting walking speed (or calculated work created air movements) in [m/s]
#' @param v a numeric value presenting relative air velocity(0.4 to 18 m/s) in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @returns  returns required recovery time in [hours]
#' @examples calcRT(90, 0, 25, 25, 8, 0.2, 0.4, 50, 1.5)
#' @references ISO 11079, 2007-12-15, ERGONOMICS OF THE THERMAL ENVIRONMENT - DETERMINATION AND INTERPRETATION OF COLD STRESS WHEN USING REQUIRED CLOTHING INSULATION (IREQ) AND LOCAL COOLING EFFECTS
#' @note The authors disclaim all obligations and liabilities for damages arising from the use or attempted use of the information, including, but not limited to, direct, indirect, special and consequential damages, and attorneys' and experts' fees and court costs. Any use of the information will be at the risk of the user.
#' @author Developed by Ingvar Holmer and Hakan O. Nilsson, 1990 in java and transferred to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @export
calcRT <- function(M, W, ta, tr, p, w, v, rh, clo) {
  if (M <= 58) {
    M <- 58
  }
  if (M >= 400) {
    M <- 400
  }
  if (w <= 0.0052 * (M - 58)) {
    w <- 0.0052 * (M - 58)
    w <- round((w * 10) / 10)
  }
  if (w >= 1.2) {
    w <- 1.2
  }
  if (v <= 0.4) {
    v <- 0.4
  }
  if (v >= 18) {
    v <- 18
  }
  clo <- clo * 0.155
  Ia <- (0.092 * exp(-0.15 * v - 0.22 * w) - 0.0045)
  calculation <- 0
  while (calculation < 2) {
    calculation <- calculation + 1
    # Calculation of Tsk (C) and wetness (%)
    # For RTneutral!
    Tsk <- 35.7 - 0.0285 * M
    wetness <- 0.001 * M
    # Calculation of Tex (C) and Pex , Psks , Pa (Pa)
    Tex <- 29 + 0.2 * ta
    Pex <- 0.1333 * exp(18.6686 - 4030.183 / (Tex + 235))
    Psks <- 0.1333 * exp(18.6686 - 4030.183 / (Tsk + 235))
    Pa <- (rh / 100) * 0.1333 * exp(18.6686 - 4030.183 / (ta + 235))
    # Calculation of S (W/m2), Rt (m2kPa/W), fcl (n.d.), hr W/m2C with stepwise iteration
    Tcl <- ta
    hr <- 3
    S <- -40
    ArAdu <- 0.77
    factor <- 100
    Iclr <- clo # Initial values !
    Balance <- 1
    while (abs(Balance) > 0.01) {
      fcl <- 1 + 1.97 * Iclr
      Iclr <- ((clo + 0.085 / fcl) * (0.54 * exp(-0.15 * v - 0.22 * w) * (p^0.075) - 0.06 * log(p) + 0.5) -
        (0.092 * exp(-0.15 * v - 0.22 * w) - 0.0045) / fcl)
      Rt <- (0.06 / 0.38) * (Ia + Iclr)
      E <- wetness * (Psks - Pa) / Rt
      Hres <- 1.73e-02 * M * (Pex - Pa) + 1.4e-03 * M * (Tex - ta)
      Tcl <- Tsk - Iclr * (M - W - E - Hres - S)
      hr <- 5.67e-08 * 0.95 * ArAdu * (exp(4 * log(273 + Tcl)) - exp(4 * log(273 + tr))) / (Tcl - tr)
      hc <- 1 / Ia - hr
      R <- fcl * hr * (Tcl - tr)
      C <- fcl * hc * (Tcl - ta)
      Balance <- M - W - E - Hres - R - C - S
      if (Balance > 0) {
        S <- S + factor
        factor <- factor / 2
      } else {
        S <- S - factor
      }
    }
    DLE <- -40 / S
  }
  if (DLE >= 0) {
    message("CALCULATION INVALID! (Negative body heat storage)")
  } else {
    RTneutral <- abs(DLE) * 10 / 10
    RTneutral <- round(RTneutral, 2)
    cat("Required recovery time in (hours): ", RTneutral)
  }
}
