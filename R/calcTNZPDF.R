#' @title Calculate values related to TNZ approach
#' @description \code{calcTNZPDF} calculates the distance from the thermoneutral zone, either skin temperature or room air related. Also calculates the probability function (PDF) of the thermoneutral zone.
#' @aliases clacTNZ
#' @aliases tnzpdf
#' @aliases TNZPDF
#' @aliases TPDF
#' @aliases tpdf
#' @usage calcTNZPDF(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh,
#' fBasMet = "rosa", fSA = "duBois", percCov = 0, TcMin = 36, TcMax = 38,
#' plotZone = FALSE, gridTaMin = 20, gridTaMax = 30, gridTskMin = 30, gridTskMax = 42,
#' gridTa = 1000, gridTsk = 1000, sa = 1.86, IbMax = 0.124, IbMin = 0.03, alphaIn = 0.08,
#' metMin = 55.3, metMax = 57.3, metDiff = .1, forPDF = FALSE, metAdapt = "none", trm = 15, TcPreAdapt = 37.2)
#' @usage TNZPDF(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh,
#' fBasMet = "rosa", fSA = "duBois", percCov = 0, TcMin = 36, TcMax = 38,
#' plotZone = FALSE, gridTaMin = 20, gridTaMax = 30, gridTskMin = 30, gridTskMax = 42,
#' gridTa = 1000, gridTsk = 1000, sa = 1.86, IbMax = 0.124, IbMin = 0.03, alphaIn = 0.08,
#' metMin = 55.3, metMax = 57.3, metDiff = .1, forPDF = FALSE, metAdapt = "none", trm = 15, TcPreAdapt = 37.2)
#' @usage tnzpdf(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh,
#' fBasMet = "rosa", fSA = "duBois", percCov = 0, TcMin = 36, TcMax = 38,
#' plotZone = FALSE, gridTaMin = 20, gridTaMax = 30, gridTskMin = 30, gridTskMax = 42,
#' gridTa = 1000, gridTsk = 1000, sa = 1.86, IbMax = 0.124, IbMin = 0.03, alphaIn = 0.08,
#' metMin = 55.3, metMax = 57.3, metDiff = .1, forPDF = FALSE, metAdapt = "none", trm = 15, TcPreAdapt = 37.2)
#' @usage TPDF(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh,
#' fBasMet = "rosa", fSA = "duBois", percCov = 0, TcMin = 36, TcMax = 38,
#' plotZone = FALSE, gridTaMin = 20, gridTaMax = 30, gridTskMin = 30, gridTskMax = 42,
#' gridTa = 1000, gridTsk = 1000, sa = 1.86, IbMax = 0.124, IbMin = 0.03, alphaIn = 0.08,
#' metMin = 55.3, metMax = 57.3, metDiff = .1, forPDF = FALSE, metAdapt = "none", trm = 15, TcPreAdapt = 37.2)
#' @usage tpdf(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh,
#' fBasMet = "rosa", fSA = "duBois", percCov = 0, TcMin = 36, TcMax = 38,
#' plotZone = FALSE, gridTaMin = 20, gridTaMax = 30, gridTskMin = 30, gridTskMax = 42,
#' gridTa = 1000, gridTsk = 1000, sa = 1.86, IbMax = 0.124, IbMin = 0.03, alphaIn = 0.08,
#' metMin = 55.3, metMax = 57.3, metDiff = .1, forPDF = FALSE, metAdapt = "none", trm = 15, TcPreAdapt = 37.2)
#' @param ht a numeric value presenting body height in [cm].
#' @param wt a numeric value presenting body weight in [kg].
#' @param age a numeric value presenting the age in [years].
#' @param gender a numeric value presenting sex (female = 1, male = 2)
#' @param clo a numeric value presenting clothing insulation level in [clo].
#' @param vel a numeric value presenting air velocity in [m/s].
#' @param tskObs a numeric value presenting actual mean skin temperature in [degree C].
#' @param taObs a numeric value presenting actual air temperature in [degree C].
#' @param met a numeric value presenting metabolic rate (activity related) in [met].
#' @param rh a numeric value presenting realtive humidity in [\%].
#' @param fBasMet a string presenting the method of calculating basal metbolic rate. Needs to be one of "rosa", "harris", "miflin", "fixed", or "direct". Fixed will result in the value of 58.2 W/m2. Direct requires definition of metMin and metMax.
#' @param fSA a string presenting the method of calculating the surface area. Needs to be one of "duBois", "mosteller", or "direct".
#' @param percCov a numeric value between 0 and 1 presenting the percentage of the body covered by clothes in [\%].
#' @param TcMin a numeric value presenting the minimum allowed core temperature in [degree C].
#' @param TcMax a numeric value presenting the maximum allowed core temperature in [degree C].
#' @param plotZone a boolean variable TRUE or FALSE stating, wether TNZ should be plotted or not.
#' @param gridTa a numeric value defining the grid size in Ta dimension.
#' @param gridTsk a numeric value defining the grid size in Tsk dimension.
#' @param gridTaMin a numeric value defining the minimum grid value for Ta, ambient temperature, in [degree C].
#' @param gridTaMax a numeric value defining the maximum grid value for Ta, ambient temperature, in [degree C].
#' @param gridTskMin a numeric value defining the minimum grid value for Tsk, skin temperature, in [degree C].
#' @param gridTskMax a numeric value defining the maximum grid value for Tsk, skin temperature, in [degree C].
#' @param sa a numeric value for surface area (only used with method fSA: direct) in [m2].
#' @param IbMin a numeric value for minimum body tissue insulation in [m2K/W].
#' @param IbMax a numeric value for maximum body tissue insulation in [m2K/W].
#' @param alphaIn a numeric value for alpha (if 0, alpha will be calculated according to Fanger.
#' @param metMin a numeric value for minimum metabolic rate (only used with method fBasMet:direct) in [W/m2].
#' @param metMax a numeric value for maximum metabolic rate (only used with method fBasMet:direct) in [W/m2].
#' @param metDiff a numeric value for difference between minimum and maximum metabolic rate (not used with method fBasMet:direct) in [W/m2].
#' @param forPDF a boolean value. If TRUE, matrix for drawing of PDF will be output, if FALSE, values for dTNZ and others will be output.
#' @param metAdapt a string presenting the method of calculating the surface area. Needs to be one of 'Hori', 'Q10', 'ATHB', or 'none'. NOTE: all methods applied here still in development and need further validation.
#' @param trm numerical value presenting the running mean outdoor temperature in [degree C]. Only used with metAdapt: Hori and ATHB.
#' @param TcPreAdapt numerical value presenting the initial core temperature before adaptation in [degree C]. Only used with metAdapt: Q10.
#' @details The percentage of the body covered by clothes can be estimated e.g. based on ISO 9920 Appendix H (Figure H.1).  A typical winter case leads to a value of around .86, in the summer case this goes down to values around .68.
#' @returns \code{calcTNZPDF} returns either a dataframe suitbale to draw the pdf of TNZ (by setting forPDF to TURE) or a dataframe with the columns dTNZ, dTNZTs, dTNZTa and others.  Thereby
#' \item{dTNZ }{The absolute distance to the centroid of the thermoneutral zone}
#' \item{dTNZTs }{Relative value of distance assuming skin temperature to be dominant for sensation}
#' \item{dTNZTa }{Relative value of distance assuming ambient temperature to be dominant for sensation}
#' @examples
#' ## Calculate and draw pdf of TNZ for a young non-obese male
#' longTcYoungMale <- calcTNZPDF(ht = 178, wt = 70, age = 30, gender = 2, clo = 0.5,
#'                              vel = 0.2, tskObs = 36.2, taObs = 26, met = 1,
#'                              rh = 50, fBasMet = "rosa", fSA = "duBois", percCov = 0.6,
#'                              TcMin = 36, TcMax = 38, plotZone = FALSE, gridTaMin = 20, gridTaMax = 30,
#'                              gridTskMin = 20, gridTskMax = 42, gridTa = 1000, gridTsk = 1000, sa = 2.0335, IbMax = 0.124,
#'                              IbMin = 0.03, alphaIn = 0, metMin = 55.3, metMax = 57.3, metDiff = 0.1, forPDF = TRUE)
#'
#' plot(density(longTcYoungMale$X2), main="", xlim=c(14,36), ylim=c(0,.50),
#'     xlab="Operative temperature [degree C]")
#' @seealso  \code{\link{calcdTNZ}}
#' @author Marcel Schweiker and Boris Kingma
#' @references Schweiker, M., Huebner, G. M., Kingma, B. R. M., Kramer, R., and Pallubinsky, H. Drivers of diversity in human thermal perception - A review for holistic comfort models. Temperature, 2018, 1 - 35.
#' @references Kingma, B. R., Schweiker, M., Wagner, A. and van Marken Lichtenbelt, W. D. Exploring the potential of a biophysical model to understand thermal sensation Proceedings of 9th Windsor Conference: Making Comfort Relevant Cumberland Lodge, Windsor, UK, 2016.
#' @references Kingma, B. and van Marken Lichtenbelt, W. Energy consumption in buildings and female thermal demand Nature. Clim. Change, 2015, 5, 1054 - 1056.
#' @references Kingma, B. R.; Frijns, A. J.; Schellen, L. and van Marken Lichtenbelt, W. D. Beyond the classic thermoneutral zone. Temperature, 2014, 1, 142 - 149.
#' @note This function was used for the review paper by Schweiker et al. (2018) (see reference above). Some of the equations implemented are still to be validated further - therefore, use this function and its parameters with great care.
#' @note This function is not (yet) implemented in \code{calcComfInd}, \code{calcdTNZ} is applied there.
#' @export





calcTNZPDF <- function(ht, wt, age, gender, clo, vel, tskObs, taObs, met,
                       rh, fBasMet = "rosa", fSA = "duBois", percCov = 0,
                       TcMin = 36, TcMax = 38, plotZone = FALSE, gridTaMin = 20, gridTaMax = 30,
                       gridTskMin = 30, gridTskMax = 42, gridTa = 1000, gridTsk = 1000,
                       sa = 1.86, IbMax = 0.124, IbMin = 0.03, alphaIn = 0.08, metMin = 55.3, metMax = 57.3, metDiff = .1, forPDF = FALSE,
                       metAdapt = "none", trm = 15, # metAdapt == c("Hori","Q10", "ATHB")
                       TcPreAdapt = 37.2
)
{
  if (fSA == "duBois") {
    sa <- (wt^0.425 * ht^0.725) * 0.007184
  }    else if (fSA == "mosteller") {
    sa <- (wt^0.5 * ht^0.5)/60
  }	else if (fSA == "direct") {
    sa <- sa
  }    else {
    stop(paste("error: ", fSA, " is not a valid method! Use one out of 'duBois', 'mosteller', or 'direct'",
               sep = ""))
  }
  if (fBasMet == "rosa") { #kcal/day
    if (gender == 1) { # female
      basMet <- 447.593 + (9.247 * wt) + (3.098 * ht) -
        (4.33 * age)
    }        else {
      basMet <- 88.362 + (13.397 * wt) + (4.799 * ht) -
        (5.677 * age)
    }
  }    else if (fBasMet == "harris") {
    if (gender == 1) {
      basMet <- 655.0955 + (9.5634 * wt) + (1.8496 * ht) -
        (4.6756 * age)
    }        else {
      basMet <- 66.473 + (13.7516 * wt) + (5.0033 * ht) -
        (6.755 * age)
    }
  }    else if (fBasMet == "miflin") {
    if (gender == 1) {
      basMet <- (10 * wt) + (6.25 * ht) - (5 * age) - 161
    }        else {
      basMet <- (10 * wt) + (6.25 * ht) - (5 * age) + 5
    }
  }    else if (fBasMet == "fixed") {
    basMet <- 1730.643
  } else if(fBasMet == "direct")  {
    basMet <- 1730.643
  }    else {
    stop(paste("error: ", fBasMet, " is not a valid method! Use one out of 'rosa', 'harris', 'miflin', 'fixed', or 'direct'.",
               sep = ""))
  }
  basMet <- 4184/86400 * basMet # [W] convert kcal/day to J/day to W (divide by 86400 = 60*60*24 seconds)

  offseqi <- floor(gridTskMin) #Ts
  offseqj <- floor(gridTaMin) #Ta

  rangei <- ceiling(gridTskMax) - floor(gridTskMin)
  rangej <- ceiling(gridTaMax)  - floor(gridTaMin)

  seqi <- seq(0, rangei, length.out = gridTsk)
  seqj <- seq(0, rangej, length.out = gridTa)

  if( alphaIn == 0 ){
    calculateAlpha <- 1
  } else {
    calculateAlpha <- 0
    alpha <- alphaIn
  }

  gammac <- 0.00750061683
  lambda <- 2.2
  velStill <- 100 * 0.05
  A <- sa
  dTNZHori <- wert <- dTNZVert <- dTNZ <- dTNZTs <- dTNZTa <- NA
  iCL <- 0.155 * clo
  vel <- max(vel, 0.05)
  va <- vel * 100

  if (fBasMet == "rosa") {
    mMin <- basMet * (met - metDiff) #[W]
    mMax <- basMet * (met + metDiff)
  } else if(fBasMet == "direct")  {
    mMin <- metMin * sa
    mMax <- metMax * sa
  } else {
    mMin <- basMet * 5/4 * (met - metDiff)
    mMax <- basMet * 5/4 * (met + metDiff)
  }

  if (metAdapt == "Hori") {
    dMetadaptHori <- 0.208*(trm-15)*1.16222 #W/m2 # -15 to set to 0 for case of trm 15 and get difference (not absolute value) for trm 29
    mMin <- mMin - (dMetadaptHori * sa)
    mMax <- mMax - (dMetadaptHori * sa)
  } else if (metAdapt == "Q10") {
    # Q10 effect
    # R2 = R1*Q10^((T2-T1)/10 degree)
    # R2/R1 = Q10^((T2-T1)/10 degree)
    # METadaptQ10 <- MET*2.3^((TcAdapt - Tc)/10)
    mMin <- mMin*2.3^((mean(c(TcMin, TcMax)) - TcPreAdapt)/10)
    mMax <- mMax*2.3^((mean(c(TcMin, TcMax)) - TcPreAdapt)/10)
  } else if (metAdapt == "ATHB") {
    METadaptPhys <- 0.017756
    dMETadaptPhys <- ifelse (((trm - 18) * METadaptPhys) < 0, 0, ((trm - 18) * METadaptPhys))
    mMin <- mMin - (dMETadaptPhys * basMet)
    mMax <- mMax - (dMETadaptPhys * basMet)
  } else if (metAdapt == "none") {
    mMin <- mMin
    mMax <- mMax
  } else {
    stop(paste("error: ", metAdapt, " is not a valid method! Use one out of 'Hori', 'Q10', 'ATHB', or 'none'.",
               sep = ""))
  }

  iBodyMax <- IbMax
  iBodyMin <- IbMin

  Tc <- Q <- data.frame(matrix(ncol = length(seqi), nrow = length(seqj)))
  # Ta <- matrix(rep((seqj + offseqj), length(seqi)), length(seqj),
  # length(seqi))
  # Ts <- matrix(rep((seqi + offseqi), each = length(seqj)),
  # length(seqj), length(seqi))
  Ta <- matrix(rep((seqj + offseqj), length(seqi)), length(seqj),
               length(seqi))
  Ts <- matrix(rep((seqi + offseqi), each = length(seqj)),
               length(seqj), length(seqi))

  if( calculateAlpha == 1 ){ # %calculate alpha according to Fanger
    mTot <- mean(c(mMin, mMax), na.rm=T)
    PaRes <- 0.001 * calcVapourpressure(Ta, rh/100)
    Cres <- 0.0014 * (mTot / A) * ( 34 - Ta )
    Eres <- 0.0173 * (mTot / A) * ( 5.87 - PaRes )
    Qrsp <- Cres + Eres
    alpha <- Qrsp / mTot

    mMaxM <- (1-alpha) * mMax
    mMinM <- (1-alpha) * mMin;

    TsMin <- TcMin - mMaxM*iBodyMax/A;
    TsMax <- TcMax - mMinM*iBodyMin/A;

    # Ts_r = TsMax - TsMin;
    # Ts_rc = (IbMax-IbMin)/Ts_r;
  } else if (calculateAlpha == 0 ){
    TsMin <- TcMin - (1 - alpha) * mMax * iBodyMax/A
    TsMax <- TcMax - (1 - alpha) * mMin * iBodyMin/A
    # TsMin <- TcMin - mMax * iBodyMax/A
    # TsMax <- TcMax - mMin * iBodyMin/A
  }

  a1 <- 0.61 * ((Ta + 273.15)/298)^3 # radiative part
  if (percCov > 1) {
    warning("Warning! percCov was >1 but should be between 0 and 1 maximum. The value of 1 was used for calculation. Consider dividing percCov by 100.")
    percCov <- min(percCov, 1)
  }
  a2 <- percCov * (0.19 * sqrt(velStill) * (298/(Ta + 273.15))) +
    (1 - percCov) * (0.19 * sqrt(va) * (298/(Ta + 273.15))) # convective part (va in cm/s)
  iAir <- 0.155/(a1 + a2)

  # Gagge model for evaporation
  pair <- gammac * calcVapourpressure(Ta, rh/100)
  ps <- gammac * calcVapourpressure(Ts, 1)
  ctc <- 1/iAir
  chr <- a1/0.155
  hconv <- ctc - chr
  fpcl <- 1/(1 + 0.143 * hconv * clo)
  w <- 0.06

  iBody <- ((iBodyMax - iBodyMin)/(TsMax - TsMin)) *
    (Ts - TsMin) # + iBodyMax
  # iBody <- ifelse(iBody > iBodyMax, iBodyMax, iBody)
  # iBody <- ifelse(iBody < iBodyMin, iBodyMin, iBody)
  iBody <- min(iBodyMax, max( (iBodyMax-iBody), iBodyMin ))

  Emax <- lambda * hconv * (ps - pair) * fpcl # lambda = lewis

  Qe <- w * Emax # A * w * Emax
  Qrc <- (A/(iCL + iAir)) * (Ts - Ta)
  # Tc <- Ts + (iBody/(1 - alpha)) * ((Ts - Ta)/(iCL + iAir) +
  # (Qe/A))
  Tc <- (iBody) * ((Ts - Ta)/(iCL + iAir) + Qe) + Ts
  Q <- Qrc + A*Qe
  colnames(Tc) <- colnames(Q) <- seqi + offseqi
  rownames(Tc) <- rownames(Q) <- seqj + offseqj

  #View(Q)

  Q[Tc > TcMax] <- NA
  Q[Tc < TcMin] <- NA
  Tc[Tc > TcMax] <- NA
  Tc[Tc < TcMin] <- NA
  taGr <- seqj + offseqj
  TsGr <- seqi + offseqi
  if (plotZone == TRUE) {
    image(taGr, TsGr, as.matrix(Tc), col = "gray", xlab = "Ambient temperature [degree C]",
          ylab = "Mean skin temperature [degree C]", xlim = c(5,
                                                              40), ylim = c(20, 42))
  }
  Qtnz <- Q

  ### new
  Tc[Q < mMin] <- NA
  Tc[Q > mMax] <- NA

  Qtnz[Q < (1 - alpha) * mMin] <- NA
  Qtnz[Q > (1 - alpha) * mMax] <- NA

  if(forPDF == TRUE) { #| forPDF == "BOTH"){
    ### transform to long format Tc
    longTc <- melt(t(Tc))
    longTc <- na.omit(longTc)
    longTc

  } else {

    # Qtnz[Q < mMin] <- NA
    # Qtnz[Q > mMax] <- NA
    if (plotZone == TRUE) {
      image(taGr, TsGr, as.matrix(Qtnz), col = "black", add = T)
    }
    QtnzTs <- Qtnz
    listTs <- NA
    g <- 1
    for (i in 1:ncol(Qtnz)) {
      for (j in 1:nrow(Qtnz)) {
        if (!is.na(Qtnz[j, i])) {
          QtnzTs[j, i] <- as.numeric(colnames(Qtnz)[i])
          listTs[g] <- as.numeric(colnames(Qtnz)[i])
          g <- g + 1
        }
      }
    }
    tskCentroid <- median(listTs, na.rm = T)
    Qtnztop <- Qtnz
    listTop <- NA
    g <- 1
    for (i in 1:ncol(Qtnz)) {
      for (j in 1:nrow(Qtnz)) {
        if (!is.na(Qtnz[j, i])) {
          Qtnztop[j, i] <- as.numeric(row.names(Qtnz)[j])
          listTop[g] <- as.numeric(row.names(Qtnz)[j])
          g <- g + 1
        }
      }
    }
    topCentroid <- median(listTop, na.rm = T)
    dTNZ <- round(sqrt((taObs - topCentroid)^2 + (tskObs - tskCentroid)^2),
                  2)
    dTNZTs <- round(tskObs - tskCentroid, 2)
    dTNZTa <- round(taObs - topCentroid, 2)
    tskCentroid <- round(tskCentroid, 2)
    topCentroid <- round(topCentroid, 2)
    tskCentroidMin <- min(listTs, na.rm = T)
    tskCentroidMax <- max(listTs, na.rm = T)
    topCentroidMin <- min(listTop, na.rm = T)
    topCentroidMax <- max(listTop, na.rm = T)
    topCentroidTscMin <- min(Qtnztop[, which.min(abs(as.numeric(colnames(Qtnztop))-tskCentroid))], na.rm = T)
    topCentroidTscMax <- max(Qtnztop[, which.min(abs(as.numeric(colnames(Qtnztop))-tskCentroid))], na.rm = T)


    #histogram(longTc$Var2, ylab="density [%]", xlab="Operative temperature [degree C]", nint=20)

    data.frame(dTNZ, dTNZTs, dTNZTa, tskCentroid, tskCentroidMin,
               tskCentroidMax, topCentroid, topCentroidMin, topCentroidMax,
               topCentroidTscMin, topCentroidTscMax)
  }
}
TNZPDF <- calcTNZPDF
tnzpdf <- calcTNZPDF
tpdf <- calcTNZPDF
TPDF  <- calcTNZPDF

