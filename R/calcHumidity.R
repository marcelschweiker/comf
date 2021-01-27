#' @title Calculating Various Humidity Related Values.
#' @description This set of functions calculates different humidity related values based on the given entities.
#' @aliases calcHumidity
#' @aliases calcDewp
#' @aliases calcEnth
#' @aliases calcHumx
#' @aliases calcMixR
#' @aliases calcRH
#' @aliases calcSVP
#' @aliases calcVP
#' @aliases calcVapourpressure
#' @usage calcDewp(ta, rh)
#' @usage calcEnth(ta, rh, pb)
#' @usage calcHumx(ta, rh)
#' @usage calcMixR(ta, rh, pb)
#' @usage calcRH(ta, mr, pb)
#' @usage calcSVP(ta)
#' @usage calcVP(ta, mr, pb)
#' @usage calcVapourpressure(ta, rh)
#' @param ta a numeric value or vector presenting air temperature in [degree C].
#' @param rh a numeric value or vector presenting relative humidity in [\%], except for \code{calcVapourpressure}, where it must be in decimal (e.g. 0.5).
#' @param mr a numeric value or vector presenting the mixIng ratio in [g/kg.
#' @param pb a numeric value or vector presenting barometric pressure in [torr].
#' @details The length of the arguments must be either the same or they must have the length one and one common second length.
#' @returns \code{calcDewp} returns the dew point temperature in [degree C]
#' @returns \code{calcEnth} returns a single value or a vector of values of enthalpy in [J]
#' @returns \code{calcHumx} returns a single value or a vector of values of the humidex of air [ ]
#' @returns \code{calcMixR} returns a single value or a vector of mixIng ratio in [g/kg]
#' @returns \code{calcRH} returns a single value or a vector of relative humidities in [\%]
#' @returns \code{calcSVP} returns a single value or a vector of saturation vapor pressure in [kpa]
#' @returns \code{calcVP} returns a single value or a vector of vapor pressure in [kpa]
#' @returns \code{calcVapourpressure} returns a single value or a vector of vapor pressure in [kpa]
#' @examples
#' ## Calc single value of absolute humidity
#' ta <- 25
#' rh <- 50
#' calcMixR(ta, rh, 760)
#' ## Calc set of values of absolute humidity
#' ta <- 25:30
#' rh <- 50
#' calcMixR(ta, rh, 760)
#' ## Calculating dew point temperature with single values for ta and rh
#' calcDewp(25, 50)
#' ## Calculating dew point temperature with a vector of values for ta and a single value for rh
#' calcDewp(25:29, 50)
#' ## Calc single value of enthalpy
#' ta <- 25
#' rh <- 50
#' calcEnth(ta, rh, 760)
#' ## Calc set of values of enthalpy
#' ta <- 25:30
#' rh <- 50
#' calcEnth(ta, rh, 760)
#' @author Michael Kleber (code and documentation), Marcel Schweiker (documentation).
#' @references Rajib Ranaa, Brano Kusya, Raja Jurdaka, Josh Wallb and Wen Hua, Feasibility analysis of using humidex as an indoor thermal comfort predictor, Energy and Buildings 64 (2013) 17-25.
#' @references Masterton, J. M., and Richardson, F. A., Humidex a method of quantifying humandiscomfort due to excessive heat and humidity, clI 1-79. Downsview, Ont: Environment Canada. Atmosheric Environment Service, 1979.
#' @export

# mixIng ratio of water in dry air
calcMixR <- function(ta, rh, pb){
  1000*((exp((17.62*ta)/(243.12+ta))*611.2*(rh/100))/(461.51*(ta+273.15)))/(((pb/760*101325)-(exp((17.62*ta)/(243.12+ta))*611.2*(rh/100)))/(287.058*(ta+273.15)))
}

# enthalpy of air
calcEnth <- function(ta, rh, pb){
  1.006*ta + (calcMixR(ta, rh, pb)/1000)*(1.86*ta + 2500)
}

# dewpoint of air
calcDewp <- function(ta, rh){
  (rh/100) ^ (1/8.02)*(109.8+ta)-109.8
}

# humidex of air
calcHumx <- function(ta, rh){
  ta+5/9*(6.11*exp(5417.753*(1/273.15-1/(calcDewp(ta, rh)+273.15)))-10)
}

# saturation vapor pressure
calcSVP <- function(ta){
  6.1078*10^((7.5*ta)/(237.3+ta))*100
}

# vapor pressure
calcVP <- function(ta, mr, pb){
  (mr/1000*(pb/760*101325)*462.51*(ta+273.15))/(mr/1000*(ta+273.15)*462.51+(ta+273.15)*287.058)
}

# relative humidity from air temperature, mixIng ratio and barometric pressure
calcRH <- function(ta, mr, pb){
  calcVP(ta, mr, pb)/calcSVP(ta)*100
}

# vapour pressure for pdftnz
calcVapourpressure <- function(ta, rh){ # Ta in degree C and rh in decimal (e.g. 0.5)
  psat <- 100 * exp(18.965 - 4030/(ta + 235))
  rh * psat
}







