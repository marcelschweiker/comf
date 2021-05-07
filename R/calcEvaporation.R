#' @title Calculate evaporative heat loss.
#' @description Function to evaporative heat loss.
#' @aliases calcEvap
#' @usage calcEvaporation(err_cr, err_sk, tsk, ta, rh, ret, height=1.72, weight=74.43, equation="dubois", age=20)
#' @usage calcEvap(err_cr, err_sk, tsk, ta, rh, ret, height=1.72, weight=74.43, equation="dubois", age=20)
#' @param err_cr Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param err_sk Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param tsk Skin temperatures in [Degree Celcius]
#' @param ta Air temperatures at local body segments in [Degree Celcius]
#' @param rh  Relative humidity at local body segments [%]
#' @param ret Total evaporative thermal resistances [m2.K/W]
#' @param height Body height [m]. The default is 1.72.
#' @param weight Body weight [kg]. The default is 74.43.
#' @param equation
#' @param age Age [years]. The default is 20.
#' @returns   wet : array Local skin wettedness [-].
#' e_sk : array Evaporative heat loss at the skin by sweating and diffuse [W].
#' e_max : array Maximum evaporative heat loss at the skin [W].
#' e_sweat : TYPE Evaporative heat loss at the skin by only sweating [W].
#' @example
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export

calcEvaporation <- function(err_cr, err_sk, tsk, ta, rh, ret, height=1.72, 
                            weight=74.43, equation="dubois", age=20){
  error <- calcERRORsig(err_cr, err_sk)  # Thermoregulation signals
  wrms <- error$wrms
  clds <- error$clds
  bsar <- calcBSArate(height, weight, equation) # BSA rate
  bsa <- BSAst * bsar  # BSA
  p_a <- calcAntoine(ta)*rh/100  # Saturated vapor pressure of ambient [kPa]
  p_sk_s <- calcAntoine(tsk)  # Saturated vapor pressure at the skin [kPa]

  e_max <- (p_sk_s - p_a) / ret * bsa  # Maximum evaporative heat loss

  # SKINS
  skin_sweat <- c(
    0.064, 0.017, 0.146, 0.129, 0.206,
    0.051, 0.026, 0.0155, 0.051, 0.026, 0.0155,
    0.073, 0.036, 0.0175, 0.073, 0.036, 0.0175)
  sig_sweat <- (371.2*err_cr[1]) + (33.64*(wrms-clds))
  sig_sweat <- max(sig_sweat, 0)
  sig_sweat <- sig_sweat * bsar

  # Signal decrement by aging
  if (age < 60){
    sd_sweat <- c(rep(1,17))
  }
  else{ #age >= 60
    sd_sweat <- c(0.69, 0.69, 0.59, 0.52, 0.40,
    0.75, 0.75, 0.75, 0.75, 0.75, 0.75,
    0.40, 0.40, 0.40, 0.40, 0.40, 0.40)
    }
  e_sweat <- skin_sweat * sig_sweat * sd_sweat * 2^((err_sk)/10)
  wet <- 0.06 + 0.94*(e_sweat/e_max)
  wet <- pmin(wet, 1)  # Wettedness' upper limit
  e_sk <- wet * e_max
  e_sweat <- (wet - 0.06) / 0.94 * e_max  # Effective sweating
  return(list(wet=wet,e_sk=e_sk, e_max=e_max, e_sweat=e_sweat))
}
