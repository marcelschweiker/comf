# new function for calculating TNZ, dTNZ, and PDFtnz
# get PDF of tnz

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
