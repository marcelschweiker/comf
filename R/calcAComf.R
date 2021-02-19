#' @title Adaptive Comfort or Neutral Temperatures
#' @description \code{calctadapt} are three functions to calculate adaptive comfort or neutral temperatures based on a given outdoor temperature value.
#' @aliases calctadapt
#' @aliases calctAdapt
#' @aliases calctAdapt15251
#' @aliases calctAdaptASHRAE
#' @aliases calctnAuliciems
#' @aliases calctnHumphreysNV
#' @aliases calctnHumphreysAC
#' @aliases tadapt
#' @aliases tAdapt15251
#' @aliases tAdaptASHRAE
#' @aliases tnAuliciems
#' @aliases tnHumphreysNV
#' @aliases tnHumphreysAC
#' @usage calctAdapt15251(trm = 20)
#' @usage calctAdaptASHRAE(tmmo)
#' @usage calctnAuliciems(ta, tmmo)
#' @usage calctnHumphreysNV(tmmo)
#' @usage calctnHumphreysAC(tmmo)
#' @param ta numerical value presenting the indoor air temperature in [degree C].
#' @param trm numerical value presenting the running mean outdoor temperature in [degree C].
#' @param tmmo numerical value presenting the mean monthly outdoor temperature in [degree C].
#' @note The difference between \code{calctnHumphreysNV} and \code{calctnHumphreysAC} is that the former was found for natural ventilated buildings (NV), while the latter was found for climate-controlled buildings (AC).
#' @returns returns the adaptive comfort or neutral temperature with respect to the given outdoor temperature value
#' @examples ## define variable
#' trm <- 21.2
#' ## calculate adaptive comfort temperature
#' calctAdapt15251(trm)
#' @author Code implemented in to R by Marcel Schweiker. Further contribution by Sophia Mueller and Shoaib Sarwar.
#' @seealso see also \code{\link{calcComfInd}}
#' @references \code{calctAdapt15251} is based on DIN EN 15251 Indoor environmental input parameters for design and assessment of energy performance of buildings addressing indoor air quality, thermal environment, lighting and acoustics; German version EN 15251:2012 2012.
#' @references \code{calcAdaptASHRAE} is based on Brager & de Dear (2001) Climate, comfort, & natural ventilation: a new adaptive comfort standard for ASHRAE standard 55.
#' @references \code{calctnAuliciems} is based on Auliciems (1981) Psycho-physiological criteria for global thermal zones of building design.
#' @references \code{calctnHumphreysNV} and \code{calctnHumphreysAC} are based on Humphreys (1978) Outdoor temperatures and comfort indoors. Batiment International, Building Research and Practice, Taylor and Francis.
#' @export



calctAdapt15251 <- function(trm=20){
  data.frame(tAdapt15251 = 0.33*trm + 18.8)
}

calctAdaptASHRAE <- function(tmmo){
  data.frame(tAdaptASHRAE = 0.33*tmmo + 17.8)
}

calctnAuliciems <- function(ta, tmmo){
  data.frame(tnAuliciems = 9.22+0.48*ta+0.14*tmmo)
}

calctnHumphreysNV <- function(tmmo){
  data.frame(tnHumphreysNV = .534*tmmo + 11.9)
}

calctnHumphreysAC <- function(tmmo){
  data.frame(tnHumphreysAC = 23.9+.295*(tmmo-22)*exp(-((tmmo-22)/(24*2 ^ .5)) ^ 2))
}

