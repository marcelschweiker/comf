library(ggplot2)
library(dplyr)

#air temperature range over which calculations are performed [oC]
tempAirInit = seq(from = 10, to = 34.5, by = 0.5)

#%RH range over which calculations are performed [%RH/100]
RHPsyInit = seq(from = 0, to = 0.99, by = 0.01)

#metabolic rate for a person in [W/m2]. default value is 69.8 W/m2, equivalent to 1.2 met (the other unit for metabolic rate)
MROrig = 1.2

#do not change, changes MR_orig inputs in 'met' to W/m2
MR= MROrig*58.2

#skin wettedness parameter, [unitless]
#0.06 for dry skin, accounts for diffusion through skin
#0.8 is the practical upper bound for very sweaty skin
w=0.06

#air speed in [m/s]
v=0.3

#the air temperature minus the mean radiant temperature (MRT) [K]
#this is specified to calculate a wind speed for thermal comfort
#values > 0 are for an air temperature greater than the MRT
#values < 0 are for an air temperature lower than the MRT
dep = 0.0000

#emissivity of human skin [unitless]
#values in the literature vary between 0.95 to 0.98
E=0.95

#Stephan Botlztmans constant
o = 0.00000005670367

#empirical ratio for a seated person
ArAd = 0.7

#empirical ratio for calculating the evaporative "heat transfer" coefficient
LR = 16.5

#define psychrometric temp bounds for drawing %RH lines
temperature = seq(from = 5, to = 39.9, by = 0.1)

#redefine temp and %RH arrays
tempAir = matrix(array(), nrow=50, ncol = 100)
RHPsy = matrix(array(), nrow=50, ncol=100)

#skin temp calculation
tempSkin = matrix(array(), nrow=50, ncol=100)

#vapor pressure of water on skin's surface
PSatSkinPsy =  matrix(array(), nrow=50, ncol=100)

#mapping %RH matrix to specific humidity for plotting
psySat = matrix(array(), nrow=50, ncol=100)

#vapor pressure of water in air
PSatAirPsy = matrix(array(), nrow=50, ncol=100)

#free convection heat transfer coefficient for seated person
hCFreePsy = matrix(array(), nrow=50, ncol=100)

#free convection around the human body
QConvFreePsy = matrix(array(), nrow=50, ncol=100)

#free convection evaporative heat transfer coefficient
hEFreePsy = matrix(array(), nrow=50, ncol=100)

#associated evaporative "heat transfer" about the human body (free convection)
QEvapFreePsy = matrix(array(), nrow=50, ncol=100)

#solved mean radiant temperature required for thermal comfort taking free convection into account
TMRTPsy = matrix(array(), nrow=50, ncol=100)

#forced convection around the human body
QConvForcedPsy = matrix(array(), nrow=50, ncol=100)

#associated evaporative "heat transfer" about the human body (free convection)
QEvapForcedPsy = matrix(array(), nrow=50, ncol=100)

#solved mean radiant temperature required for thermal comfort taking forced convection into account
TMRTForcedPsy = matrix(array(), nrow=50, ncol=100)

#calculate mean radiant temperature when "dep" parameter is specified to solve for required air speed
tempMRT = matrix(array(), nrow=50, ncol=100)

#linearizing radiant heat transfer coefficient
hRForcedV = matrix(array(), nrow=50, ncol=100)

#linearized radiant heat transfer
QRadForcedV = matrix(array(), nrow=50, ncol=100)

#required air speed for thermal comfort using the 'dep' parameter to fix the mean radiant temperature
vForcedV = matrix(array(), nrow=50, ncol=100)

#2D relative humidity array dimension constant
dim1 = nrow(RHPsy)

#2D air temperature array dimension constant 
dim2 = nrow(tempAir)

#clausius-clapyron constants
a=17.08
b=234.1
saturation = 1000*0.62198*exp(77.345+0.0057*(temperature+273.15)-7235/(temperature+273.15))/(101325*((temperature+273.15)^8.2)-exp(77.345+0.0057*(temperature+273.15)-7235/(temperature+273.15)))

#for an arbitrary fixed comfort zone box calculation
#Mapped everything from relative humidity to absolute humidity
#BL = Bottom Left ...
BL = 0.3*1000*0.62198*exp(77.345+0.0057*(21+273.15)-7235/(21+273.15))/(101325*((21+273.15)^8.2)-exp(77.345+0.0057*(21+273.15)-7235/(21+273.15)))
TL = 0.6*1000*0.62198*exp(77.345+0.0057*(21+273.15)-7235/(21+273.15))/(101325*((21+273.15)^8.2)-exp(77.345+0.0057*(21+273.15)-7235/(21+273.15)))
BR = 0.3*1000*0.62198*exp(77.345+0.0057*(27+273.15)-7235/(27+273.15))/(101325*((27+273.15)^8.2)-exp(77.345+0.0057*(27+273.15)-7235/(27+273.15)))
TR = 0.6*1000*0.62198*exp(77.345+0.0057*(27+273.15)-7235/(27+273.15))/(101325*((27+273.15)^8.2)-exp(77.345+0.0057*(27+273.15)-7235/(27+273.15)))
comfort = seq(from = 21, to = 26.9, by = 0.1)

#forced convection heat transfer coefficient calculation
hCForcedPsy = 10.1*(v^0.61)

#forced convection evaporative heat transfer coefficient
hEForcedPsy = hCForcedPsy*LR

#ARRAY DECLARATIONS AND CALCULATIONS
for (indexI in 1:50) {
  tempAirInitVal = tempAirInit[indexI]
  tempSkinVal = tempAirInitVal*.3812+22.406
  PSatSkinPsyVal = (2.718^(77.3450+0.0057*(tempSkinVal+273.15)-7235/(tempSkinVal+273.15)))/(((tempSkinVal+273.15)^8.2))/1000
  psySatValPre = 1000*0.62198*exp(77.345+0.0057*(tempAirInitVal+273.15)-7235/(tempAirInitVal+273.15))/(101325*((tempAirInitVal+273.15)^8.2)-exp(77.345+0.0057*(tempAirInitVal+273.15)-7235/(tempAirInitVal+273.15)))
  PSatAirPsyValPre = (2.718^(77.3450+0.0057*(tempAirInitVal+273.15)-7235/(tempAirInitVal+273.15)))/(((tempAirInitVal+273.15)^8.2))/1000
  hCFreePsyVal = 0.78*(abs(tempSkinVal-tempAirInitVal)^0.56)
  QConvFreePsyVal = hCFreePsyVal*(tempSkinVal-tempAirInitVal)
  hEFreePsyVal = hCFreePsyVal*LR
  tempMRTVal = tempAirInitVal - dep
  hRForcedVVal = 4*E*o*ArAd*((273.15+(tempSkinVal+tempMRTVal)/2)^3)
  QRadForcedVVal = hRForcedVVal*E*(tempSkinVal-tempMRTVal)
  
  for (indexJ in 1:100) {
    RHPsyInitVal = RHPsyInit[indexJ]
    PSatAirPsyVal = (2.718^(77.3450+0.0057*(tempAirInitVal+273.15)-7235/(tempAirInitVal+273.15)))/(((tempAirInitVal+273.15)^8.2))/1000*RHPsyInitVal
    QEvapFreePsyVal = hEFreePsyVal*w*(PSatSkinPsyVal-PSatAirPsyVal)
    QConvForcedPsyVal = hCForcedPsy*(tempSkinVal-tempAirInitVal)
    QEvapForcedPsyVal = hEForcedPsy*w*(PSatSkinPsyVal-PSatAirPsyVal)
    vForcedVal = (((MR-QRadForcedVVal)/(10.1*(LR*w*(PSatSkinPsyVal-PSatAirPsyVal)+(tempSkinVal-tempAirInitVal))))^(1/0.61))
    
    tempAir[indexI, indexJ] = tempAirInitVal
    RHPsy[indexI, indexJ] = RHPsyInitVal
    tempSkin[indexI, indexJ] = tempSkinVal
    PSatSkinPsy[indexI, indexJ] = PSatSkinPsyVal
    psySat[indexI, indexJ] = psySatValPre*RHPsyInitVal
    PSatAirPsy[indexI, indexJ] = PSatAirPsyVal
    hCFreePsy[indexI, indexJ] = hCFreePsyVal
    QConvFreePsy[indexI, indexJ] = QConvFreePsyVal
    hEFreePsy[indexI, indexJ] = hEFreePsyVal
    QEvapFreePsy[indexI, indexJ] = QEvapFreePsyVal
    TMRTPsy[indexI, indexJ] = ((((tempSkinVal+273.15)^4)-((MR-QEvapFreePsyVal-QConvFreePsyVal)/E/o/ArAd))^0.25)-273.15
    QConvForcedPsy[indexI, indexJ] = QConvForcedPsyVal
    QEvapForcedPsy[indexI, indexJ] = QEvapForcedPsyVal
    TMRTForcedPsy[indexI, indexJ] = ((((tempSkinVal+273.15)^4)-((MR-QEvapForcedPsyVal-QConvForcedPsyVal)/E/o/ArAd))^0.25)-273.15
    tempMRT[indexI, indexJ] = tempMRTVal
    hRForcedV[indexI, indexJ] = hRForcedVVal
    QRadForcedV[indexI, indexJ] = QRadForcedVVal
    vForcedV[indexI, indexJ] = vForcedVal
  } 
}

#set transparency
alph = 0.6
forcedVal = tail(sort(vForcedV),350)
df = tbl_df(data.frame(temperature, saturation))

plot1 = ggplot(df, aes(x = temperature, y = saturation)) + ylim(0, 50) + 
  geom_line() + 
  geom_line(y = saturation*0.9, alpha = alph) + 
  geom_line(y = saturation*0.8, alpha = alph) + 
  geom_line(y = saturation*0.7, alpha = alph) + 
  geom_line(y = saturation*0.6, alpha = alph) + 
  geom_line(y = saturation*0.5, alpha = alph) + 
  geom_line(y = saturation*0.4, alpha = alph) + 
  geom_line(y = saturation*0.3, alpha = alph) + 
  geom_line(y = saturation*0.2, alpha = alph) + 
  geom_line(y = saturation*0.1, alpha = alph)

plot2 = plot.new()

if(dep==0){
  levelsContour = seq(0.1, 2, length.out= 150)
  filteredVforcedV = vForcedV[,-(51:100)]
  for (i in 2:ncol(tempAir)) {
    plot2 = plot2 + filled.contour(x = tempAir[,i], y = psySat[,i], 
                            z = filteredVforcedV, levels = levelsContour, 
                            color.palette=colorRampPalette(c("blue","cyan", "yellow","orange","red")) 
                            )
  }
}

plot2
