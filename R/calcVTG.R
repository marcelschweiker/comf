#' @title PPD with Vertical Air Temperature Gradient
#' @description Function to calculate vertical air temperature gradient using the predicted percentage of dissatisfied.
#' @aliases  vairtepgrad
#' @aliases  vtg
#' @aliases  vairtmpgrad
#' @usage calcVTG(ta, tr, vel, rh, clo, met, v_tmp_grad)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param v_tmp_grad vertical temperature gradient between the feet and the head [degree C/m]
#' @details  Calculates the percentage of thermally dissatisfied persons with a vertical temperature gradient between feet and head. Applicable only for velocity(vel) < 0.2 m/s
#' @returns Predicted Percentage of Dissatisfied with vertical temperature gradient in [\%]
#' @returns Acceptability in [boolean]
#' @references
#' Original code in Python by Tartarini & Schiavon (2020) <doi:10.1016/j.softx.2020.100578>
#' @examples calcVTG(25, 25, 0.1, 50, 0.5, 1.2, 7)
#' # returns Vertical Air Temperature Gradient:12.4, Acceptability:FALSE
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @export

calcVTG <- function(ta, tr, vel, rh, clo, met, v_tmp_grad) {
  tsv <- calcPMV(ta = ta, tr = tr, vel = vel, rh = rh, clo = clo, met = met)
  tsv <- round(tsv, 1)
  numerator <- exp(0.13 * (tsv - 1.91)**2 + 0.15 * v_tmp_grad - 1.6)
  ppdval <- (numerator / (1 + numerator) - 0.345) * 100


  ppdval <- round(ppdval, 1)

  acceptability <- ppdval <= 5

  if (vel > 0.2) {
    warning("Velocity(vel) should be less than or equal to 0.2.")
  }
  return(list(
    PPD_vg = ppdval,
    Acceptability = acceptability
  ))
}
