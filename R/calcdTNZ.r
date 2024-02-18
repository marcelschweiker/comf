# Function: dTNZ ################
###########################################
#' dTNZ, the Distance from the Thermoneutral Zone
#'
#' @description calcdTNZ calculates the distance from the thermoneutral zone,
#' either skin temperature or room air related.
#' 
#' @usage 
#' calcdTNZ(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh, deltaT =.1, 
#' fBasMet = "rosa", fSA = "duBois", percCov = 0, TcMin = 36, TcMax = 38, 
#' plotZone = FALSE)
#' 
#' @param ht a numeric value presenting body height in [cm]
#' @param wt a numeric value presenting body weight in [kg]
#' @param age a numeric value presenting the age in [years]
#' @param gender a numeric value presenting sex (female = 1, male = 2)
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param tskObs a numeric value presenting actual mean skin temperature in [degree C]
#' @param taObs a numeric value presenting air temperaturein [degree C]
#' @param met a numeric value presenting metabolic rate (activity related) in [met]
#' @param rh a numeric value presenting realtive humidity in [\%]
#' @param deltaT a numeric value presenting the resolution of the matrix to be used
#' @param fBasMet a string presenting the method of calculating basal metbolic 
#' rate. Needs to be one of "rosa", "harris", "miflin", or "fixed". Fixed will result in the value of 58.2 W/m2. 
#' @param fSA a string presenting the method of calculating the surface area. 
#' Needs to be one of "duBois" or "mosteller".
#' @param percCov a numeric value between 0 and 1 presenting the percentage of 
#' the body covered by clothes in [\%]
#' @param TcMin a numeric value presenting the minimum allowed core temperature 
#' in [degree C].
#' @param TcMax a numeric value presenting the maximum allowed core temperature 
#' in [degree C].
#' @param plotZone a boolean variable TRUE or FALSE stating, wether TNZ should 
#' be plotted or not.
#'
#' @details The percentage of the body covered by clothes can be estimated e.g. 
#' based on ISO 9920 Appendix H (Figure H.1). A typical winter case leads to a 
#' value of around .86, in the summer case this goes down to values around .68.
#' 
#' @return \code{calcdTNZ} returns a dataframe with the columns dTNZ, dTNZTs, dTNZTa. Thereby \cr
#'  \code{dTNZ}    The absolute distance to the centroid of the thermoneutral zone \cr
#'  \code{dTNZTs}  Relative value of distance assuming skin temperature to be dominant for sensation\cr
#'  \code{dTNZTa}  Relative value of distance assuming ambient temperature to be dominant for sensation \cr
#' 
#' @note 
#' This function was used in earlier versions of TNZ calculation (see references
#' above). The newest version is \code{calcTNZPDF}.In case one of the variables
#' is not given, a standard value will be taken from a list (see 
#' \code{\link{createCond}} for details.
#' 
#' @author Marcel Schweiker and Boris Kingma
#' 
#' @references 
#' Kingma, Schweiker, Wagner & van Marken Lichtenbelt 
#' Exploring the potential of a biophysical model to understand thermal sensation 
#' Proceedings of 9th Windsor Conference: Making Comfort Relevant Cumberland 
#' Lodge, Windsor, UK, 2016.
#' Kingma & van Marken Lichtenbelt (2015) <doi:10.1038/nclimate2741>
#' Kingma, Frijns, Schellen & van Marken Lichtenbelt (2014) <doi:10.4161/temp.29702>
#' 
#' @seealso see also \code{\link{calcTNZPDF}} and \code{\link{calcComfInd}}
#' 
#' @export
#'
#' @examples
#' ## Calculate all values
#' calcdTNZ(171, 71, 45, 1, .6, .12, 37.8, 25.3, 1.1, 50) 

calcdTNZ <- function(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh, deltaT =.1, fBasMet = "rosa", fSA = "duBois", percCov = 0, TcMin = 36, TcMax = 38, plotZone = FALSE){		
  
  if (fSA == "duBois"){
    # calculation of surface area according to duBois
    sa <- (wt ^ .425 * ht ^ .725) * .007184
  } else if (fSA == "mosteller"){
    # calculation of surface area according to mosteller
    sa <- (wt ^ .5 * ht ^ .5) / 60
  } else {
    stop(paste("error: ", fSA, " is not a valid method! Use one out of 'duBois', or 'mosteller'", sep = ""))
  }
  
  if (fBasMet == "rosa"){
    # calculation of basal metabolic rate according to Harris-Benedict equations revised by Roza and Shizgal in 1984
    if (gender == 1){ # female
      basMet <- 447.593 + (9.247 * wt) + (3.098 * ht) - (4.330 * age)
    } else {
      basMet <- 88.362 + (13.397 * wt) + (4.799 * ht) - (5.677 * age)
    }
  } else if (fBasMet == "harris"){
    # calculation of basal metabolic rate according to original Harris-Benedict equations 
    if (gender == 1){ # female
      basMet <- 655.0955 + (9.5634 * wt) + (1.8496 * ht) - (4.6756 * age)
    } else {
      basMet <- 66.4730 + (13.7516 * wt) + (5.0033 * ht) - (6.7550 * age)
    }
  } else if (fBasMet == "miflin"){
    # calculation of basal metabolic rate according to original Harris-Benedict equations 
    if (gender == 1){ # female
      basMet <- (10 * wt) + (6.25 * ht) - (5 * age) - 161
    } else {
      basMet <- (10 * wt) + (6.25 * ht) - (5 * age) + 5
    }
  } else if (fBasMet == "fixed"){
    basMet <- 1730.643
  } else {
    stop(paste("error: ", fBasMet, " is not a valid method! Use one out of 'rosa', 'harris', 'miflin', or 'fixed'.", sep = ""))
  }
  
  basMet <- 4184 / 86400 * basMet # [W]
  #basMet <- basMet * 5 / 4 # adjust for basMet = .8 met
  
  # definition of resolution of combinations
  mult    <- 1 / deltaT
  seqi    <- seq(0, 31, deltaT)
  seqj    <- seq(0, 31, deltaT)
  offseqi <- 19 #Ts
  offseqj <- 9 #Ta
  
  alpha  <- 0.08 # []
  
  gammac <- 0.00750061683 # [mmHg/pa]
  lambda <- 2.2 # [degree C/mmHg] Lewis relation
  
  # TcMin  <- 36 # [degree C]
  # TcMax  <- 38 # [degree C]
  
  velStill <- 100 * 0.05
  
  A <- sa
  dTNZHori <- wert <- dTNZVert <- dTNZ <- dTNZTs <- dTNZTa <- NA
  
  iCL <- 0.155 * clo # [m2 degree C/W]
  
  vel <- max(vel, 0.05) # set minimum vel to .05 m/s
  va <- vel * 100 # [cm/s] convert va to cm/s
  
  if (fBasMet == "rosa"){
    mMin <- basMet * (met-.1) # [W]
    mMax <- basMet * (met+.1) # [W]
  } else {
    mMin <- basMet * 5 / 4 * (met-.1) # [W]
    mMax <- basMet * 5 / 4 * (met+.1) # [W]
  }
  
  iBodyMax <- 0.112 # [m2 degree C/W] see Veicsteinas et al.
  iBodyMin <- 0.032 # [m2 degree C/W]
  TsMin <- TcMin - (1 - alpha) * mMax * iBodyMax / A # [degree C] 
  TsMax <- TcMax - (1 - alpha) * mMin * iBodyMin / A # [degree C] 
  
  # define empty matrix
  Tc <- Q <- data.frame(matrix(ncol = length(seqi), nrow = length(seqj)))
  
  Ta <- matrix(rep((seqj + offseqj), length(seqi)), length(seqj), length(seqi))
  Ts <- matrix(rep((seqi + offseqi), each = length(seqj)), length(seqj), length(seqi))
  
  iBody <- iBodyMax + ((iBodyMax - iBodyMin) / (TsMin - TsMax)) * (Ts - TsMin)
  iBody <- ifelse(iBody > iBodyMax, iBodyMax, iBody)
  iBody <- ifelse(iBody < iBodyMin, iBodyMin, iBody)
  
  #iAir <- 1 / ((0.19 * sqrt(va) * (298 / (Ta + 273.15))) + (0.61 * ((Ta + 273.15) / 298) ^ 3)) 
  
  #a1 <- 0.61 * ((Ta + 273.15) / 298) ^ 3 # radiative
  #a2 <- 0.8 * (0.19 * sqrt(velStill) * (298 / (Ta + 273.15))) + 0.2 * (0.19 * sqrt(va) * (298 / (Ta + 273.15)))
  
  #iAir <- .155 / (a1 + a2) # weighted average due to body not completely exposed to air velocity
  
  a1 <- 0.61 * ((Ta + 273.15) / 298) ^ 3 # radiative
  
  if(percCov >1){
    warning("Warning! percCov was >1 but should be between 0 and 1 maximum. The value of 1 was used for calculation. Consider dividing percCov by 100.")
    percCov <- min(percCov,1) # limit percCov to max of 1
  }
  
  a2 <- percCov * (0.19 * sqrt(velStill) * (298 / (Ta + 273.15))) + # covered part
    (1-percCov) * (0.19 * sqrt(va) * (298 / (Ta + 273.15))) # uncovered part
  
  iAir <- .155 / (a1 + a2) # to adjust from clo unit to m2K/w
  
  ctc <- 1 / iAir
  chr <- a1 / .155
  
  hconv <- ctc - chr
  
  ps <- gammac * 100 * exp(18.965 - 4030 / (Ts + 235))
  pair <- gammac * rh * exp(18.965 - 4030 / (Ta + 235))
  
  fpcl <- 1 / (1 + .143 * hconv * clo)
  
  #if(fSkinWet == "const"){ # experimental for testing other method of calcuting skin wettedness. So far not leading to reliable results.
  w <- 0.06 # skin wettedness fraction []
  # } else if(fSkinWet == "Gagge"){
  # wCol = NA
  # for(i in 1:length(seqj)){
  # TaTemp <- seqj[i] + offseqj
  # wCol[i] 	   <- calcSkinWettedness(TaTemp, TaTemp, vel, rh, clo, met)
  # rm(TaTemp)
  # }
  # w <- matrix(rep(wCol, length(seqi)), length(seqj), length(seqi))
  # }
  
  Qe  <- A * w * lambda * hconv * (ps - pair) * fpcl
  Qrc <- (A / (iCL + iAir)) * (Ts - Ta) 
  
  # calculating and storing values for Tc
  Tc <- Ts + (iBody / (1 - alpha)) * ((Ts - Ta) / (iCL + iAir) + (Qe / A))
  # calculation and storing values for Q: metabolic heat production
  Q <- Qrc + Qe
  
  colnames(Tc) <- colnames(Q) <- seqi + offseqi # 
  rownames(Tc) <- rownames(Q) <- seqj + offseqj #
  
  #setting all to NA, where Tc is unrealistic 36 < Tc < 38
  Q[Tc > TcMax] <- NA
  Q[Tc < TcMin] <- NA
  
  Tc[Tc > TcMax] <- NA
  Tc[Tc < TcMin] <- NA
  
  # defintion of vectors used for image()
  taGr <- seqj + offseqj 
  TsGr <- seqi + offseqi 
  
  if(plotZone == TRUE){
    image(taGr, TsGr, as.matrix(Tc), col="gray", xlab="Ambient temperature [degree C]", ylab="Meanskin temperature [degree C]", xlim=c(5,40), ylim=c(20,42))
  }
  
  Qtnz <- Q
  Qtnz[Q < (1 - alpha) * mMin] <- NA
  Qtnz[Q > (1 - alpha) * mMax] <- NA
  if(plotZone == TRUE){
    image(taGr, TsGr, as.matrix(Qtnz), col="black", add=T)
  }
  
  #########################
  # from here get dTNZ (vertical + horizontal)
  #########################
  
  # transfer values in Qtnz to values of columns (tsk)
  QtnzTs <- Qtnz
  
  listTs <- NA
  g <- 1
  
  for (i in 1:ncol(Qtnz)){
    for (j in 1:nrow(Qtnz)){
      if (!is.na(Qtnz[j, i])){
        QtnzTs[j, i] <- as.numeric(colnames(Qtnz)[i])
        listTs[g] <- as.numeric(colnames(Qtnz)[i])
        g <- g + 1
      }
    }
  }
  #QtnzTs
  tskCentroid <- median(listTs, na.rm=T)
  
  # transfer values in Qtnz to values of columns (tsk)
  Qtnztop <- Qtnz
  
  listTop <- NA
  g <- 1
  
  for (i in 1:ncol(Qtnz)){
    for (j in 1:nrow(Qtnz)){
      if (!is.na(Qtnz[j, i])){
        Qtnztop[j, i] <- as.numeric(row.names(Qtnz)[j])
        listTop[g] <- as.numeric(row.names(Qtnz)[j])
        g <- g + 1
      }
    }
  }
  #Qtnztop
  topCentroid <- median(listTop,na.rm=T)
  
  #%calculate distance to TNZ (dTNZ)
  dTNZ <- round(sqrt((taObs-topCentroid) ^ 2 + (tskObs-tskCentroid) ^ 2 ), 2); # abs value of distance
  dTNZTs <- round(tskObs-tskCentroid, 2) # rel value of distance assuming tskin to be dominant for sensation
  dTNZTa <- round(taObs-topCentroid, 2) # rel value of distance assuming tambient to be dominant for sensation
  tskCentroid <- round(tskCentroid, 2)
  topCentroid <- round(topCentroid, 2)
  
  tskCentroidMin <- min(listTs, na.rm=T)
  tskCentroidMax <- max(listTs, na.rm=T)
  
  topCentroidMin <- min(listTop, na.rm=T)
  topCentroidMax <- max(listTop, na.rm=T)
  topCentroidTscMin <- min(Qtnztop[,which(colnames(Qtnztop)==round_any(tskCentroid,deltaT))], na.rm=T)
  topCentroidTscMax <- max(Qtnztop[,which(colnames(Qtnztop)==round_any(tskCentroid,deltaT))], na.rm=T)
  
  #points(topCentroid,tskCentroid, col="red")
  
  data.frame(dTNZ, dTNZTs, dTNZTa, tskCentroid, tskCentroidMin, tskCentroidMax, topCentroid, topCentroidMin, topCentroidMax, topCentroidTscMin, topCentroidTscMax)
  
  #Qx <<- Q
  #TcX <<- Tc
}
