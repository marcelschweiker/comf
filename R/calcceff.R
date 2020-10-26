#' @title Calculate Cooling Effect
#' @description Function to calculate cooling effect using the function standard effective temperature.
#' @usage calcCE(ta, tr, vel, rh, clo = 0.5, met = 1, wme = 0)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param wme a numeric value presenting external work in [met]
#' @details The CE of the elevated air speed is the value that, when subtracted equally from both the average air temperature and the mean radiant temperature, yields the same SET under still air as in the first SET calculation under elevated air speed. The cooling effect is calculated only for air speed higher than 0.1 m/s.
#' @returns ce - Cooling Effect in [degree C]
#' @examples calcCE(25,25,.3,50)
#' @export



calcCE <- function(ta,tr,vel,rh,clo=.5,met=1,wme=0){
  if (vel <= 0.2){
    return(0)
  }


  still_air_threshold <- 0.1
  initial_set_tmp = calcSET(ta=ta, tr=tr, vel=vel, rh=rh, met=met, clo=clo, wme=wme)

  f <- function(x){
    return (calcSET(ta - x, tr - x, vel=still_air_threshold, rh=rh, met=met, clo=clo, wme=wme)
            - initial_set_tmp)

  }
  ce = bisect(f,1,20)
  ce = lapply(ce, round,2)
  return(ce)

}

