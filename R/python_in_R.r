library("reticulate")

utciModel = py_run_file('~/Documents/GitHub/comf/R/models.py')

#for UTCI
ta = tr = c(0,20,40)
vel = c(0,5,10)
rh = c(0,50,100)
utciValue = expand.grid(ta = ta, tr = tr, vel = vel, rh = rh)
utciPython = c()
utciR = c()
for(i in 1:nrow(utciValue)){
  row <- utciValue[i,]
  utciPython = append(utciPython, utciModel$utci(row$ta, row$tr, row$vel, row$rh))
  utciR = append(utciR, calcUTCI(row$ta, row$tr, row$vel, row$rh))
}
utciValue$UTCI_Python = utciPython
utciValue$UTCI_R = utciR

#for solar gain
solAlt = c(0, 45, 90) 
solAzi = c(0, 90, 180) 
solRadDir = c(200, 1100, 2000) 
solTrans = fSvv = fBes = asw = c(0,0.5,1)
posture= c("standing", "supine", "seated")
solarGainR = c()
solarGainPython = c()
solarGainMatch = c()
solarGainValue = expand.grid(solAlt = solAlt, solAzi = solAzi, 
                             solRadDir = solRadDir, solTrans = solTrans,
                             fSvv = fSvv, fBes = fBes, asw = asw,
                             posture = posture)
for(i in 1:nrow(solarGainValue)){
  row = solarGainValue[i,]
  rResult = calcSolarGain(row$solAlt, row$solAzi,row$solRadDir, row$solTrans,
                          row$fSvv, row$fBes, row$asw,row$posture)
  pythonResult = utciModel$solar_gain(row$solAlt, row$solAzi,row$solRadDir, row$solTrans,
                                  row$fSvv, row$fBes, row$asw,row$posture)
  rStringResult = cat("erf:", rResult[1], "delta_mrt:", rResult[2])
  solarGainR = append(solarGainR, rStringResult)
  solarGainPython = append(solarGainPython, pythonResult)
  solarGainMatch = (rStringResult == pythonResult)
}
solarGainValue$SolarGainPython = solarGainPython
solarGainValue$SolarGainR = solarGainR
solarGainValue$ResultMatched = solarGainMatch
