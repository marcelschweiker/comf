#' Calculates the Heat Index (HI).
#' 
#' @param tdb float value that dry bulb air temperature, default in [°C] in [°F] if `units` = 'IP'
#' @param rh float value that is relative humidity, [%]
#' @param round is boolean value, default True. If True rounds output value, if False it does not round it
#' @param units is {'SI', 'IP'}, select the SI (International System of Units) or the IP (Imperial Units) system.
#' 
#' @details It combines air temperature and relative humidity to determine an apparent temperature. 
#' The HI equation [12]_ is derived by multiple regression analysis in temperature and relative 
#' humidity from the first version of Steadman’s (1979) apparent temperature (AT) [13]_.
#'
#' @return hi, a float value, is the Heat Index, default in [°C] in [°F] if `units` = 'IP'
#' @references Steadman’s (1979) apparent temperature (AT)
#' @examples
#' calcHeatIndex(tdb=25, rh=50)
#' @author Code implemented into R by Yiqing Zhang.
#' @export

calcHeatIndex <- function(tdb, rh, round_value = TRUE, units = "SI") {
  if (units == "SI") {
    # from doi: 10.1007/s00484-011-0453-2
    hi <- -8.784695 + 1.61139411 * tdb + 2.338549 * rh - 0.14611605 * tdb * rh
    hi <- hi - 1.2308094e-2 * tdb^2 - 1.6424828e-2 * rh^2
    hi <- hi + 2.211732e-3 * tdb^2 * rh + 7.2546e-4 * tdb * rh^2
    hi <- hi - 3.582e-6 * tdb^2 * rh^2
  } else {
    # from doi: 10.1007/s00484-021-02105-0
    hi <- -42.379 + 2.04901523 * tdb + 10.14333127 * rh
    hi <- hi - 0.22475541 * tdb * rh - 6.83783e-3 * tdb^2
    hi <- hi - 5.481717e-2 * rh^2
    hi <- hi + 1.22874e-3 * tdb^2 * rh + 8.5282e-4 * tdb * rh^2
    hi <- hi - 1.99e-6 * tdb^2 * rh^2
  }

  if (round_value) {
    return(round(hi, 1))
  } else {
    return(hi)
  }
}
