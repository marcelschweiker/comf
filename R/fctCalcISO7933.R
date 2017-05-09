# v1.0 done by Michael Kleber

# Function returns:
#
# Tre        - final rectal temperature [degree C]
# SWtotg     - total water loss [g]
# Dlimtre    - time when limit for rectal temperature is reached [min]
# Dlimloss50 - time when limit for water loss Dmax50 (7.5 percent of body mass of an average person) is reached [min]
# Dlimloss95 - time when limit for water loss Dmax95 (5 percent of body mass of 95 percent of the working people) is reached [min]
# Cres       - convective heat flow at respiration [W/(m*m)]
# Eres	 - evaporative heat flow at respiration [W/(m*m)]
# Ep         - predicted evaporative heat flow [W/(m*m)]
# SWp		 - predicted sweating rate [W/(m*m)]
# Texp	 - temperature of the exhaled air [degree C]
# Tskeq	 - skin Temperature in equilibrium [degree C]
# Tsk		 - skin Temperature at the minute [degree C]
# wp		 - predicted skin wettedness [-]
#
# File contains 1 function:
#   - function(accl, posture, Ta, Pa, Tr, Va, Met, Icl, THETA, Walksp, Duration, weight, height, DRINK, Adu, spHeat, SWp, Tre, Tcr, Tsk, Tcreq, Work, imst, Ap, Fr, defspeed, defdir, HR, pb)
#       returns a data.frame with Tre, SWtotg, Dlimtre, Dlimloss50, Dlimloss95, Cres, Eres, Ep, SWp, Texp, Tskeq, Tsk, wp
#
# The code for fctCalcISO7933 is based on the code in BASIC presented in the standard ISO 7933 

# Function: fctCalcISO7933 ################
###########################################



calcIso7933 <- function(accl, posture, Ta, Pa, Tr, Va, Met, Icl, THETA, Walksp, Duration, weight, height, DRINK, Adu, spHeat, SWp, Tre, Tcr, Tsk, Tcreq, Work, imst, Ap, Fr, defspeed, defdir, HR, pb){


# INITIALISATION

# The user must make sure that, at this point in the programme,
# the following parameters are available.
# Standard values must be replaced by actual values if necessary.
# The water replacement is supposed to be sufficient so that the workers can drink freely (DRINK=1), otherwise the value DRINK=0 must be used

# from code in standard
if(missing(weight)){
	weight <- 75 # body mass kilograms
	warning("weight set to default 75 kg")
}
# from code in standard
if(missing(height)){
	height <- 1.8 # body height meters
	warning("height set to default 1.8 m")
}
if(missing(DRINK)){
	DRINK <- 1 # workers can drink as they want
	warning("DRINK set to default 1")
}

# from code in standard
if(missing(Adu)){
	Adu <- 0.202 * (weight ^ 0.425) * (height ^ 0.725)
	warning("Adu calculated by default with height and weight")
}

# from code in standard
# c_sp ist spHeat - spezifische Koerperwaerme [(W/m2)/K]
if(missing(spHeat)){
	spHeat <- 57.83 * weight / Adu
	warning("spHeat (=c_sp) calculated by default with Adu and weight")
}

# from code in standard
if(missing(SWp)){
	SWp <- 0
	warning("Swp set to default 0")
}

# from code in standard
if(missing(Tcr)){
	Tcr <- 36.8
	warning("Tcr set to default 36.8")
}

# from code in standard
if(missing(Tre)){
	Tre <- Tcr
	warning("Tre set to Tcr")
}

# from code in standard
if(missing(Tcreq)){
	Tcreq <- Tcr
	warning("Tcreq set equal Tcr")
}

if(missing(Tsk)){
	Tsk <- 34.1
	warning("Tsk set to default 34.1")
}

# following lines from code in standard
SWtot <- 0
TskTcrwg <- .3
Dlimtre <- 0
Dlimloss50 <- 0
Dlimloss95 <- 0
Dmax50 <- 0.075 * weight * 1000
Dmax95 <- 0.05 * weight * 1000

# EXPONENTIAL AVERAGING CONSTANTS
# Core temperature as a function of the metabolic rate: time constant: 10 minutes
ConstTeq <- exp(-1 / 10)
# Skin Temperature: time constant: 3 minutes
ConstTsk <- exp(-1 / 3)
# Sweat rate: time constant: 10 minutes
ConstSW <- exp(-1 / 10)

if(missing(Duration)){
	Duration <- 480 # the duration of the work sequence in minutes
	warning("Duration set to standard of 480 minutes")
}

## setting default values originally in the for loop

if(missing(Ta)){
	Ta <- 40 #air temperature degrees celsius
	warning("Ta set to default of 40 degrees celsius")
}

if(missing(Tr)){
	Tr <- Ta #mean radiant temperature degrees celsius
	warning("Tr set by default to Ta")
}

if(missing(Pa)){
	if(missing(HR))
	{
	Pa <- 2.5 #partial water vapour pressure kilopascals
	warning("Pa set by default to 2.5 kilopascals")
	}
	else
	{
		if(missing(pb))
		{
		pb <- 101325 # normal barometric pressure in [Pa]
		warning("Pa calculated by humidity ratio and standard pressure of 101325 Pa")	
		}
		Pa <- HR/1000/(0.622+HR/1000)*pb/1000
	}
}

if(missing(Va)){
	Va <- 0.3 #air velocity metres per second
	warning("Va set by default to 0.3 metres per second")
}

if(missing(Met)){
	Met <- 150 #metabolic rate Watts per square meter
	warning("Met set by default to 150 Watts per square meter")
}

if(missing(Work)){
	Work <- 0 #effective mechanical power Watts per square metre
	warning("Work set by default to 0 Watts per square metre")
}

if(missing(posture)){
	posture <- 2 #posture = 1 sitting, = 2 standing, = 3 crouching
	warning("posture set by default to 2 = standing")
}

if(missing(Icl)){
	Icl <- 0.5 #static thermal insulation clo
	warning("Icl set by default to 0.5 clo")
}

if(missing(imst)){
	imst <- 0.38 #static moisture permeability index dimensionless
	warning("imst set by default to 0.38")
}

if(missing(Ap)){
	Ap <- 0.54 #fraction of the body surface covered by the reflective clothing dimensionless
	warning("Ap set by default to 0.54")
}

if(missing(Fr)){
	Fr <- 0.97 #emissivity of the reflective clothing dimensionless (by default: Fr=0.97)
	warning("Fr set by default to 0.97")
}

if(missing(accl)){
	#code =100 if acclimatised subject, 0 otherwise
	accl <- 100 
	warning("accl set by default to 100 = acclimated subject")
}



if(missing(THETA)){
	THETA <- 0 #angle between walking direction and wind direction degrees
	defdir <- 0 #code =1 if walking direction entered, 0 otherwise
	warning("THETA set by default to 0")
	}
	else
	{
	defdir <- 1
	}

if(missing(Walksp)){
	Walksp <- 0 #walking speed metres per second
	defspeed <- 0 #code =1 if walking speed entered, 0 otherwise
	warning("Walksp set by default to 0")
	}
	else
	{
	if (Walksp == 0)
		{
		defspeed <- 0
		}
		else
		{
		defspeed <- 1
		}
	}


## start of for loop

for (time in 1:Duration)
{
	# INITIALISATION MIN PER MIN
	Tsk0 <- Tsk
	Tre0 <- Tre
	Tcr0 <- Tcr
	Tcreq0 <- Tcreq
	TskTcrwg0 <- TskTcrwg
	
	# INPUT OF THE PRIMARY PARAMETERS
	# The user must make sure that, at this point in the programme,
	# the following parameters are available. In order for the user
	# to test rapidly the programme, the data for the first case
	# in annex E of the ISO 7933 standard are introduced.
	
	# Ta = 40 #air temperature degrees celsius
	# Tr = 40 #mean radiant temperature degrees celsius
	# Pa = 2.5 #partial water vapour pressure kilopascals
	# Va = 0.3 #air velocity metres per second
	# Met = 150 #metabolic rate Watts per square meter
	# Work = 0 #effective mechanical power Watts per square metre
	# posture = 2 #Posture posture = 1 sitting, = 2 standing, = 3 crouching
	# Icl = 0.5 #static thermal insulation clo
	# imst = 0.38 #static moisture permeability index dimensionless
	# Ap = 0.54 #fraction of the body surface covered by the reflective clothing dimensionless
	# Fr = 0.97 #emissivity of the reflective clothing dimensionless (by default: Fr=0.97)
	#Ardu dimensionless
	# defspeed = 0 #code =1 if walking speed entered, 0 otherwise
	# Walksp = 0 #walking speed metres per second
	# defdir = 0 #code =1 if walking direction entered, 0 otherwise
	# THETA = 0 #angle between walking direction and wind direction degrees
	
	# accl = 100: #code =100 if acclimatised subject, 0 otherwise

	# code from standard
	# Effective radiating area of the body
	if (posture == 1) 
		{ Ardu <- 0.7 }
	if (posture == 2) 
		{ Ardu <- 0.77 }
	if (posture == 3) 
		{ Ardu <- 0.67}
	
	# EVALUATION OF THE MAXIMUM SWEAT RATE AS A FUNCTION OF THE METABOLIC RATE
	SWmax <- (Met - 32) * Adu
	if (SWmax > 400) 
		{ SWmax <- 400 }
	if (SWmax < 250) 
		{ SWmax <- 250 }

	# For acclimatised subjects (accl=100), the maximum Sweat Rate is greater by 25%
	if (accl >= 50) 
		{ SWmax <- SWmax * 1.25 }
	if (accl < 50)
		{ Wmax <- 0.85 } 
		else
		{ Wmax <- 1 }

	# from code in standard	
	# EQUILIBRIUM CORE TEMPERATURE ASSOCIATED TO THE METABOLIC RATE
	Tcreqm <- 0.0036 * Met + 36.6
	# Core temperature at this minute, by exponential averaging
	Tcreq <- Tcreq0 * ConstTeq + Tcreqm * (1 - ConstTeq)
	# Heat storage associated with this core temperature increase during the last minute
	dStoreq <- spHeat * (Tcreq - Tcreq0) * (1 - TskTcrwg0)

	# SKIN TEMPERATURE PREDICTION
	# Skin Temperature in equilibrium
	# Clothed model
	Tskeqcl <- 12.165 + .02017 * Ta + .04361 * Tr + .19354 * Pa - .25315 * Va
	Tskeqcl <- Tskeqcl + .005346 * Met + .51274 * Tre
	# Nude model
	Tskeqnu <- 7.191 + .064 * Ta + .061 * Tr + .198 * Pa - .348 * Va
	Tskeqnu <- Tskeqnu + .616 * Tre

	# Value at this minute, as a function of the clothing insulation
	if (Icl >= 0.6) 
		{ Tskeq <- Tskeqcl }
	if (Icl <= 0.2) 
		{ Tskeq <- Tskeqnu }
	# Interpolation between the values for clothed and nude subjects, if 0.2 < clo < 0.6
	if (Icl > 0.2 && Icl < 0.6)
		{ 
		Tskeq <- Tskeqnu + 2.5 * (Tskeqcl - Tskeqnu) * (Icl - 0.2)
		}
	# Skin Temperature at this minute, by exponential averaging
	#Tsk:
	Tsk <- Tsk0 * ConstTsk + Tskeq * (1 - ConstTsk)
	# Saturated water vapour pressure at the surface of the skin
	Psk <- 0.6105 * exp(17.27 * Tsk / (Tsk + 237.3))
	
	# CLOTHING INFLUENCE ON EXCHANGE COEFFICIENTS
	# Static clothing insulation
	Iclst <- Icl * 0.155
	# Clothing area factor
	fcl <- 1 + 0.3 * Icl
	# Static boundary layer thermal insulation in quiet air
	Iast <- 0.111
	# Total static insulation
	Itotst <- Iclst + Iast / fcl


	# Relative velocities due to air velocity and movements
	if (defspeed > 0) 
	{
		if (defdir == 1) 
		{
			# Unidirectional walking
			Var <- abs(Va - Walksp * cos(3.14159 * THETA / 180))
		}
		else
		{
			# Omni-directional walking
			if (Va < Walksp)  
			{ Var <- Walksp } 
			else 
			{ Var <- Va }
		}
	}
	else
	{
		# Stationary or undefined speed
		Walksp <- 0.0052 * (Met - 58)
		if (Walksp > 0.7)
		{
		Walksp <- 0.7
		}
		Var <- Va
	}

	# Dynamic clothing insulation
	# Clothing insulation correction for wind (Var) and walking (Walksp)
	Vaux <- Var
	if (Var > 3)
		{ 
		Vaux <- 3
		}
	Waux <- Walksp
	if (Walksp > 1.5)
		{
		Waux <- 1.5
		}
	CORcl <- 1.044 * exp((0.066 * Vaux - 0.398) * Vaux + (0.094 * Waux - 0.378) * Waux)
	if (CORcl > 1)
		{
		CORcl <- 1
		}
	CORia <- exp((0.047 * Var - 0.472) * Var + (0.117 * Waux - 0.342) * Waux)
	if (CORia > 1)
		{
		CORia <- 1
		}
	CORtot <- CORcl
	if (Icl <= 0.6)
		{
		CORtot <- ((0.6 - Icl) * CORia + Icl * CORcl) / 0.6
		}
	Itotdyn <- Itotst * CORtot
	IAdyn <- CORia * Iast


	Icldyn <- Itotdyn - IAdyn / fcl
	
	# Permeability index
	# Correction for wind and walking
	CORe <- (2.6 * CORtot - 6.5) * CORtot + 4.9
	imdyn <- imst * CORe
	if (imdyn > 0.9)
		{
		imdyn <- 0.9
		}
	# Dynamic evaporative resistance
	Rtdyn <- Itotdyn / imdyn / 16.7

	# HEAT EXCHANGES
	# Heat exchanges through respiratory convection and evaporation
	# temperature of the expired air
	Texp <- 28.56 + 0.115 * Ta + 0.641 * Pa
	Cres <- 0.001516 * Met * (Texp - Ta)
	Eres <- 0.00127 * Met * (59.34 + 0.53 * Ta - 11.63 * Pa)

	# Mean temperature of the clothing: Tcl
	# Dynamic convection coefficient
	Z <- 3.5 + 5.2 * Var
	if (Var > 1)
		{
		Z <- 8.7 * Var ^ 0.6
		}
	Hcdyn <- 2.38 * abs(Tsk - Ta) ^ 0.25
	if (Z > Hcdyn)
		{
		Hcdyn <- Z
		}

	auxR <- 0.0000000567 * Ardu
	FclR <- (1 - Ap) * 0.97 + Ap * Fr
	Tcl <- Tr + 0.1

###	# Tcl Iteration by GOTO replaced through repeat statement
	repeat 
	{
	# Radiation coefficient
	Hr <- FclR * auxR * ((Tcl + 273) ^ 4 - (Tr + 273) ^ 4) / (Tcl - Tr)
	Tcl1 <- ((fcl * (Hcdyn * Ta + Hr * Tr) + Tsk / Icldyn)) / (fcl * (Hcdyn + Hr) + 1 / Icldyn)
   	if (abs(Tcl - Tcl1) <= 0.001)
		{
      	break
   		}
	Tcl <- (Tcl + Tcl1) / 2
	}

	# Convection and Radiation heat exchanges
	Conv <- fcl * Hcdyn * (Tcl - Ta)
	Rad <- fcl * Hr * (Tcl - Tr)
	# Maximum Evaporation Rate
	Emax <- (Psk - Pa) / Rtdyn
	# Required Evaporation Rate
	Ereq <- Met - dStoreq - Work - Cres - Eres - Conv - Rad

	# INTERPRETATION
	# Required wettedness
	wreq <- Ereq / Emax
	# Required Sweat Rate
	# If no evaporation required: no sweat rate
	if (Ereq <= 0)	
		{ 
		Ereq <- 0
		SWreq <- 0
		} 
		else
		{
		# If evaporation is not possible, sweat rate is maximum
		if (Emax <= 0) 
			{ 
			Emax <- 0
			SWreq <- SWmax
			}
			else
			{
			# If required wettedness greater than 1.7: sweat rate is maximum
			if (wreq >= 1.7)
				{ 
				wreq <- 1.7
				SWreq <- SWmax
				}
				else
				{
				# Required evaporation efficiency
				Eveff <- (1 - wreq ^ 2 / 2)
				if (wreq > 1) 
					{
					Eveff <- (2 - wreq) ^ 2 / 2
					}
				SWreq <- Ereq / Eveff
				if (SWreq > SWmax) 
					{
					SWreq <- SWmax
					}
				}
			}
		}

	# Predicted Sweat Rate, by exponential averaging
	SWp <- SWp * ConstSW + SWreq * (1 - ConstSW)
	if (SWp <= 0) 
		{
		Ep <- 0
		SWp <- 0
		} 
		else
		{

		# Predicted Evaporation Rate
		k <- Emax / SWp
		wp <- 1
		if (k >= 0.5) 
			{ 
			wp <- -k + sqrt(k * k + 2)
			}
		if (wp > Wmax) 
			{ 
			wp <- Wmax
			}
		Ep <- wp * Emax
		}

	# Heat Storage
	dStorage <- Ereq - Ep + dStoreq

	# PREDICTION OF THE CORE TEMPERATURE
	Tcr1 <- Tcr0

	# Iteration of TskTcr GOTO replaced by repeat statement
	repeat
	{
	# Skin - Core weighting
	TskTcrwg <- 0.3 - 0.09 * (Tcr1 - 36.8)
	if (TskTcrwg > 0.3) 
		{
		TskTcrwg <- 0.3
		}
	if (TskTcrwg < 0.1)
		{ 
		TskTcrwg <- 0.1
		}
	Tcr <- dStorage / spHeat + Tsk0 * TskTcrwg0 / 2 - Tsk * TskTcrwg / 2
	Tcr <- (Tcr + Tcr0 * (1 - TskTcrwg0 / 2)) / (1 - TskTcrwg / 2)
	
	if (abs(Tcr - Tcr1) <= 0.001)
		{
		break
		}
	Tcr1 <- (Tcr1 + Tcr) / 2
	}

	# PREDICTION OF THE RECTAL TEMPERATURE
	Tre <- Tre0 + (2 * Tcr - 1.962 * Tre0 - 1.31) / 9
	if (Dlimtre == 0 && Tre >= 38)
		{
		Dlimtre <- time
		}

	# Total water loss rate during the minute (in W / m-2)
	SWtot <- SWtot + SWp + Eres
	SWtotg <- SWtot * 2.67 * Adu / 1.8 / 60
	if (Dlimloss50 == 0 && SWtotg >= Dmax50) 
		{
		Dlimloss50 <- time
		}
	if (Dlimloss95 == 0 && SWtotg >= Dmax95)
		{
		Dlimloss95 <- time
		}
	if (DRINK == 0)
		{
		Dlimloss95 <- Dlimloss95 * 0.6
		Dlimloss50 <- Dlimloss95
		}

}
# End of loop on duration

#Dlim computation
if (Dlimloss50 == 0)
	{ 
	Dlimloss50 <- Duration
	}
if (Dlimloss95 == 0) 
	{ 
	Dlimloss95 <- Duration
	}
if (Dlimtre == 0) 
	{
	Dlimtre <- Duration
	}

data.frame(Tre,SWtotg,Dlimtre,Dlimloss50,Dlimloss95,Cres,Eres,Ep,SWp,Texp,Tskeq,Tsk,wp)

# end of function
}