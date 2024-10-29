#' @title Calculates the Wind Chill Index (WCI)
#' @details Calculates the Wind Chill Index (WCI) in accordance with the
#' ASHRAE 2017 Handbook Fundamentals - Chapter 9. The wind chill index is an
#' empirical index based on cooling measurements taken on a cylindrical
#' flask partially filled with water in Antarctica (Siple and Passel 1945).
#' For a surface temperature of 33 degree, the index describes the rate of
#' heat loss from the cylinder via radiation and convection as a
#' function of ambient temperature and wind velocity.
#'
#' This formulation has been met with some valid criticism.
#' WCI is unlikely to be an accurate measure of heat loss from exposed flesh
#' which differs from plastic in terms of curvature, roughness, and radiation
#' exchange qualities, and is always below 33 Degree in a cold environment.
#'
#' @param tdb Numeric value indicating the dry bulb air temperature in [Degree].
#' @param v Numeric value indicating the wind speed 10 meters above ground level in [m/s].
#' @param round_output Logical value; if TRUE, the output is rounded to one decimal place.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{wci}}{Numeric value of the wind chill index in [W/m squre].}
#' }
#'
#' @examples wc(tdb = -5, v = 5.5)
#'
#' @references
#' ASHRAE Handbook-Fundamentals (2017). Chapter 9: Thermal Comfort.
#'
#' @export
calcWci <- function(tdb, v, round_output = TRUE) {
  wci <- (10.45 + 10 * sqrt(v) - v) * (33 - tdb)
  # The factor 1.163 converts the result to W/m squre
  wci <- wci * 1.163

  if (round_output) {
    wci <- round(wci, 1)
  }

  return(list(wci = wci))
}
