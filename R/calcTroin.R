#' @title Calculation of Radiative and Operative Temperature
#' @description The functions \code{calcTroin} calculates radiative and operative temperature based on air temperature, globe temperature, air velocity and metabolic rate. Globe temperature needs to be measured using a standard globe with a diameter of 0.15m and an emissivity of .95 (black coloured).
#' @aliases calctroin
#' @aliases Troin
#' @aliases troin
#' @usage calcTroin(vel, tg, ta, met)
#' @usage calctroin(vel, tg, ta, met)
#' @usage Troin(vel, tg, ta, met)
#' @usage troin(vel, tg, ta, met)
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param tg a numeric value or vector presenting the globe temperature in [degree C]
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param met a numeric value presenting metabolic rate in [met]
#' @details Calculation of the radiative temperature is based on ISO 7726:2001, equation (9). Calculation of operative temperature is based on ISO 7726:2001, Appendix G.3. The adjustment of air velocity to present relative air velocity based on metabolic rate is based on ISO 7730:2005 Appendix C.2.
#' @returns \code{calcTroin} returns a data.frame with radiative and operative temperature.
#' @examples ## Note: Due to random generated asv values. The values for the coefficients will not be meaningful.
#' ## Create sample data
#' ta  <- 20:24      # vector with air temperature values
#' vel <- rep(.1,5)  # vector with air velocities
#' met <- rep(1.1,5) # vector with metabolic rates
#' tg <- 25:29       # vector with globe temperature values
#'
#' calcTroin(vel, tg, ta, met)
#' @author  Marcel Schweiker. Further contribution by Shoaib Sarwar.
#' @references IsO 7726 Ergonomics of the Thermal Environment, Instruments for measuring Physical Quantities Geneva: International standard Organization, 1998.
#' @references IsO 7730 Ergonomics of the thermal environment - analytical determination and interpretation of thermal comfort using calculation of the pmv and ppd indices and local thermal comfort criteria 2005.
#' @export


calcTroin <- function(vel, tg, ta, met){

  ifelse(vel <= 0, 0, vel)
  met<-as.numeric(met*58) #w/m2
  varIn<-vel+0.0052*(met-58) # see EN IsO 7730:2005 Appendix C.2
  vara<-ifelse(varIn<0.2,0.5,ifelse(varIn>0.6,0.7,0.6)) # see appendix G.3 of EN IsO 7726:2001
  tr<-((tg+273)^4+2.5*10^8*vel^0.6*(tg-ta))^0.25-273 # see equation 9 of EN IsO 7726:2001
  to<-(vara*ta)+(1-vara)*tr # see appendix G.3 of EN IsO 7726:2001
  data.frame(tr, to)

}
calctroin <- calcTroin
Troin <- calcTroin
troin <- calcTroin

