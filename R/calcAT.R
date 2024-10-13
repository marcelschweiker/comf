#' Calculate Apparent Temperature (AT)
#'
#' This function calculates the Apparent Temperature (AT), which
#' is defined as the temperature at the reference humidity level
#' producing the same amount of discomfort as that experienced under
#' the current ambient conditions of temperature, humidity,
#' and possibly solar radiation.
#'
#' @param tdb Dry bulb air temperature in degrees Celsius.
#' @param rh Relative humidity in percent.
#' @param v Wind speed 10m above ground level in meters per second.
#' @param q Net radiation absorbed per unit area of body surface in W/m2.
#' @param round If TRUE, the output value is rounded to one decimal place.
#' @return Apparent temperature in degrees Celsius.
#' @examples
#' at(tdb = 25, rh = 30, v = 0.1) # Without solar radiation
#' at(tdb = 25, rh = 50, v = 0.1, q = 300) # With solar radiation
#' @export
calcAT <- function(tdb, rh, v, q = NULL, round = TRUE) {
  enthalpy <- function(tdb, hr) {
    cp_air <- 1005
    h_fg <- 2501000
    cp_vapour <- 1846

    h_dry_air <- cp_air * tdb
    h_sat_vap <- h_fg + cp_vapour * tdb
    h <- h_dry_air + hr * h_sat_vap

    return(round(h, 2))
  }

  t_wb <- function(tdb, rh) {
    twb <- tdb * atan(0.151977 * sqrt(rh + 8.313659)) +
      atan(tdb + rh) -
      atan(rh - 1.676331) +
      0.00391838 * rh^1.5 * atan(0.023101 * rh) -
      4.686035

    return(round(twb, 1))
  }

  t_dp <- function(tdb, rh) {
    c <- 257.14
    b <- 18.678
    d <- 234.5

    gamma_m <- log(rh / 100 * exp((b - tdb / d) * (tdb / (c + tdb))))

    return(round(c * gamma_m / (b - gamma_m), 1))
  }

  psy_ta_rh <- function(tdb, rh, p_atm = 101325) {
    p_saturation <- p_sat(tdb)
    p_vap <- rh / 100 * p_saturation
    hr <- 0.62198 * p_vap / (p_atm - p_vap)
    tdp <- t_dp(tdb, rh)
    twb <- t_wb(tdb, rh)
    h <- enthalpy(tdb, hr)

    return(list(
      p_sat = p_saturation,
      p_vap = p_vap,
      hr = hr,
      t_wb = twb,
      t_dp = tdp,
      h = h
    ))
  }

  # Convert relative humidity percentage to partial vapor pressure in hPa
  p_vap <- psy_ta_rh(tdb, rh)$p_vap / 100

  # Calculate AT based on whether solar radiation is considered
  if (!is.null(q)) {
    t_at <- tdb + 0.348 * p_vap - 0.7 * v + 0.7 * q / (v + 10) - 4.25
  } else {
    t_at <- tdb + 0.33 * p_vap - 0.7 * v - 4.00
  }

  # Optionally round the result
  if (round) {
    t_at <- round(t_at, 1)
  }

  return(t_at)
}
