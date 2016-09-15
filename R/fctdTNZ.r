#
# File contains 1 function:
#   - calcdTNZ(182, 82, 21, 1, .5, .1, 36, 20)
#       returns dTNZ, dTNZTa, dTNZTs
#
# v1.0 done by Marcel Schweiker in cooperation with B. Kingma


# Function: dTNZ ################
###########################################
# ht <- 175; wt <- 80; age <- 30; gender <- 1; clo <- .5; vel <- .1; tskObs <- 36; taObs <- 25; met <- 1.1; rh <- 50; deltaT =1; fBasMet <- "rosa"; fSA <- "duBois"	

## calcdTNZ <- function(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh, deltaT =.1, fBasMet = "rosa", fSA = "duBois", fSkinWet = "const"){ ## experimental and not leading to reliable results (s. line 122ff.)

calcdTNZ <- function(ht, wt, age, gender, clo, vel, tskObs, taObs, met, rh, deltaT =.1, fBasMet = "rosa", fSA = "duBois"){		
	 
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

	TcMin  <- 36 # [degree C]
	TcMax  <- 38 # [degree C]

	velStill <- 100 * 0.05

	A <- sa
	dTNZHori <- wert <- dTNZVert <- dTNZ <- dTNZTs <- dTNZTa <- NA

	iCL <- 0.155 * clo # [m2 degree C/W]
	va <- vel # [m/s]
	va <- max(va,.05) # set minimum va to .05
	va <- va * 100 # [cm/s] convert va to cm/s

	mMin <- basMet * 5 / 4 * (met-.1) # [W]
	mMax <- basMet * 5 / 4 * (met+.1) # [W]

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
	
	#iAir <- 1 / ((0.19 * sqrt(va) * (298 / (ta + 273.15))) + (0.61 * ((ta + 273.15) / 298) ^ 3)) 
	#a1 <- 0.8*(0.19*sqrt(velStill)*(298/(ta+273.15))) + 0.2*(0.19*sqrt(va)*(298/(ta+273.15)))
	#a2 <- (0.61*((ta+273.15)/298) ^ 3)
	#iAir <- 1/(a1+a2) # weighted average due to body not completely exposed to air velocity

	a1 <- 0.61 * ((Ta + 273.15) / 298) ^ 3 # radiative
	a2 <- 0.19 * sqrt(va) * (298 / (Ta + 273.15)) #convective
	
	iAir <- .155 / (a1 + a2) # to adjust from clo unit to m2K/w
	
	ctc <- 1 / iAir
	chr <- a1 / .155
	
	hconv <- ctc - chr
	#hconv <- 1 / iAir - (0.61 * ((Ta + 273.15) / 298) ^ 3) / 0.155 # [W/m2 degree C]
	#hconv <- 0.19*sqrt(va)*(298/(ta+273.15))

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
	
	#image(taGr, TsGr, as.matrix(Tc), col="gray", xlab="Ambient temperature [degree C]", ylab="Meanskin temperature [degree C]", xlim=c(5,40), ylim=c(20,42))

	Qtnz <- Q
	Qtnz[Q < (1 - alpha) * mMin] <- NA
	Qtnz[Q > (1 - alpha) * mMax] <- NA
	#image(taGr, TsGr, as.matrix(Qtnz), col="black", add=T)


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
		
		
