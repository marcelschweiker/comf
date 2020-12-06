library("reticulate")

utciModel = py_run_file('~/Documents/GitHub/comf/R/models.py')

#for UTCI
ta = tr = c(0,20,40)
vel = c(0,5,10)
rh = c(0,50,100)
utciTest = expand.grid(ta = ta, tr = tr, vel = vel, rh = rh)
utciPython = c()
utciR = c()
for(i in 1:nrow(utciTest)){
  row <- utciTest[i,]
  utciPythonResult = utciModel$utci(row$ta, row$tr, row$vel, row$rh)
  if(is.numeric(utciPythonResult)){
    utciPython = append(utciPython, utciPythonResult)
  }
  else{
    utciPython = append(utciPython, NA)
  }
  utciR = append(utciR, calcUTCI(row$ta, row$tr, row$vel, row$rh))
}
utciTest$UTCI_Python = utciPython
utciTest$UTCI_R = utciR

#for solar gain
solAlt = c(0,90) 
solAzi = c(0,180) 
solRadDir = c(200, 1000) 
solTrans = fSvv = fBes = asw = c(0,1)
posture= c("standing", "supine", "seated")
erfR = c()
erfPython = c()
deltaMrtR = c()
deltaMrtPython = c()
solarGainTest = expand.grid(solAlt = solAlt, solAzi = solAzi, 
                             solRadDir = solRadDir, solTrans = solTrans,
                             fSvv = fSvv, fBes = fBes, asw = asw,
                             posture = posture)
for(i in 1:nrow(solarGainTest)){
  row = solarGainTest[i,]
  rResult = calcSolarGain(row$solAlt, row$solAzi,row$solRadDir, row$solTrans,
                          row$fSvv, row$fBes, row$asw,row$posture)
  pythonResult = utciModel$solar_gain(row$solAlt, row$solAzi,row$solRadDir, row$solTrans,
                                  row$fSvv, row$fBes, row$asw,row$posture)
  erfR = append(erfR, rResult[1])
  erfPython = append(erfPython, pythonResult['erf'])
  deltaMrtR = append(deltaMrtR, rResult[2])
  deltaMrtPython = append(deltaMrtPython, pythonResult['delta_mrt'])
}
solarGainTest$erfR = erfR
solarGainTest$erfPython = erfPython
solarGainTest$deltaMrtR = deltaMrtR
solarGainTest$deltaMrtPython = deltaMrtPython
