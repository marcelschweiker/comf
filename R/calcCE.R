#' @title Cooling Effect
#' @description Function to calculate cooling effect (CE) of elevated air velocities using the standard effective temperature (SET).
#' @aliases coolingeffect
#' @aliases ce
#' @usage calcCE(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @details The CE of the elevated air velocity is the difference in SET between conditions with given air velocities and still air. The cooling effect should be calculated only for air velocities higher than 0.1 m/s.
#' @returns ce - Cooling Effect in [degree C]
#' @references
#' Original code in Python by Tartarini & Schiavon (2020) <doi:10.1016/j.softx.2020.100578>
#' @examples calcCE(25, 25, 0.3, 50, 0.5, 1) # returns Cooling Effect: 1.3
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @export


calcCE <- function(ta, tr, vel, rh, clo, met, wme = 0) {
  still_air_threshold <- 0.1
  if (vel <= still_air_threshold) {

    ce <- 0
    return(ce)
    warning('For velocity less than or equal to 0.1, cooling effect is Zero')
  }

  initial_set_tmp <- calcSET(ta = ta, tr = tr, vel = vel, rh = rh, clo = clo, met = met, wme = wme)
  f <- function(x) {
    change <- calcSET(ta - x, tr - x, vel = still_air_threshold, rh = rh, clo = clo, met = met, wme = wme)
    return(change - initial_set_tmp)
  }
  out <- tryCatch(
    {
      ce <- bisect(f, 0, 40)
      return(ce)
    },
    error = function(cond) {
      message(paste("The cooling effect could not be calculated, assuming the value 0"))
      message(cond)
      # Choose a return value in case of error
      return(0)
    }
  )
}
