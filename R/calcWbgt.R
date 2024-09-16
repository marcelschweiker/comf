#' Calculates the Wet Bulb Globe Temperature (WBGT)
#'
#' The WBGT is a heat stress index that measures the thermal
#' environment to which a person is exposed. It is compliant with ISO 7243.
#'
#' @param twb Numeric value representing natural (no forced air flow)
#' wet bulb temperature in degrees Celsius.
#' @param tg Numeric value representing globe temperature in [degree C].
#' @param tdb Numeric value representing dry bulb air temperature in [degree C].
#' @param with_solar_load Logical, TRUE if the globe sensor is exposed to sun.
#' @param round Logical, TRUE if output should be rounded. Default is TRUE.
#'
#' @details The WBGT determines the impact of heat on a person throughout
#' the course of a working day (up to 8 hours). It does not apply to very brief
#' heat exposures. It pertains to the evaluation of male and female individuals
#' who are fit for work in both indoor and outdoor occupational environments,
#' as well as other types of environments. The WBGT is defined as
#' a function of only twb and tg if the person is not exposed to direct radiant
#' heat from the sun. When exposed, tdb must also be specified.
#'
#' @return Numeric value of the Wet Bulb Globe Temperature Index in [degree C].
#' @references ISO 7243
#' @examples
#' calcWbgt(twb = 25, tg = 32, tdb = 20, with_solar_load = TRUE)
#' @author Code implemented into R by Chongyu Gan.
#' @export
calcWbgt <- function(
    twb, tg, tdb = NULL,
    with_solar_load = FALSE, round = TRUE) {
  if (with_solar_load && is.null(tdb)) {
    stop("Please enter the dry bulb air temperature")
  }

  if (with_solar_load) {
    t_wbg <- 0.7 * twb + 0.2 * tg + 0.1 * tdb
  } else {
    t_wbg <- 0.7 * twb + 0.3 * tg
  }

  if (round) {
    return(round(t_wbg, 1))
  } else {
    return(t_wbg)
  }
}
