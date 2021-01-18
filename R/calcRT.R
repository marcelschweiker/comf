calcRT <- function(M,W,Ta,Tr,p,w,v,rh,Icl){
  if (M<=58) {
    M=58
  }
  if (M>=400) {
    M=400
  }
  if (w<=0.0052*(M-58)) {
    w=0.0052*(M-58)
    w= round((w*10)/10)
  }
  if (w>=1.2) {
    w=1.2
  }
  if (v<=0.4) {
    v=0.4
  }
  if (v>=18) {
    v=18
  }
  Icl=Icl*0.155
  Ia=(0.092*exp(-0.15*v-0.22*w)-0.0045)
  calculation=0
  while(calculation<2) {
    calculation=calculation+1
    #Calculation of Tsk (C) and wetness (%)
    #For RTneutral!
    Tsk=35.7-0.0285*M
    wetness=0.001*M
    #Calculation of Tex (C) and Pex , Psks , Pa (Pa)
    Tex=29+0.2*Ta
    Pex=0.1333*exp(18.6686-4030.183/(Tex+235))
    Psks=0.1333*exp(18.6686-4030.183/(Tsk+235))
    Pa=(rh/100)*0.1333*exp(18.6686-4030.183/(Ta+235));
    # Calculation of S (W/m2), Rt (m2kPa/W), fcl (n.d.), hr W/m2C with stepwise iteration
    Tcl=Ta
    hr=3
    S=-40
    ArAdu=0.77
    factor=100
    Iclr=Icl #Initial values !
    repeat{
      fcl=1+1.97*Iclr
      Iclr=((Icl+0.085/fcl)*(0.54*exp(-0.15*v-0.22*w)*(p^0.075)-0.06*log(p)+0.5)-
              (0.092*exp(-0.15*v-0.22*w)-0.0045)/fcl);
      Rt=(0.06/0.38)*(Ia+Iclr)
      E=wetness*(Psks-Pa)/Rt
      Hres=1.73e-02*M*(Pex-Pa)+1.4e-03*M*(Tex-Ta)
      Tcl=Tsk-Iclr*(M-W-E-Hres-S)
      hr=5.67e-08*0.95*ArAdu*(exp(4*log(273+Tcl))-exp(4*log(273+Tr)))/(Tcl-Tr)
      hc=1/Ia-hr
      R=fcl*hr*(Tcl-Tr)
      C=fcl*hc*(Tcl-Ta)
      Balance=M-W-E-Hres-R-C-S
      if (Balance>0)  {
        S=S+factor
        factor=factor/2
      }
      else {
        S=S-factor;
      }
      if (abs(Balance) > 0.01){
        break
      }


    }
    DLE=-40/S
    if (DLE>=0) {
      message("CALCULATION INVALID! (Negative body heat storage)")
    }
    else{
      RTneutral = round(abs(DLE)*10)/10
    }

  }
  cat("Required recovery time in (hours): ", DLE)

}
calcRT(40,50,55,25,8,0.2,5,50,3)

