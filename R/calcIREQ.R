calcIREQ <- function(M,W,Ta,Tr,p,w,v,rh,Icl){
  S=1

  if (M <= 58){
    M= 58
  }
  if (M >= 400){
    M=400
  }
  if (Ta >= 10){
    Ta=10
  }
  # Calculation of stationary w (m/s)
  if (w <= 0.0052*(M-58)) {
    w=0.0052*(M-58)
  }
  if (w >= 1.2) {
    w=1.2
  }
  if (v <= 0.4) {
    v=0.4
  }
  if (v >= 18) {
    v=18
  }
  Icl = Icl * 0.15
  Ia =0.092*exp(-0.15 * v - 0.22 *  w) - 0.0045
  calculation=0
  while(calculation<2){
    calculation=calculation+1
    #Calculation of Tsk (C) and wetness (%)
    if (calculation==1) {
    #For IREQminimal, DLEminimal !
        Tsk=33.34-0.0354*M
        wetness=0.06
    }
    else  {
    #For IREQneutral, DLEneutral !
        Tsk=35.7-0.0285*M
        wetness=0.001*M
    }
    #Calculation of Tex (C) and Pex,Psks,Pa (Pa)
    Tex=29+0.2*Ta
    Pex=0.1333*exp(18.6686-4030.183/(Tex+235))
    Psks=0.1333*exp(18.6686-4030.183/(Tsk+235))
    Pa=(rh/100)*0.1333*exp(18.6686-4030.183/(Ta+235))
    #Calculation of IREQ (m2C/W),Rt (m2kPa/W),fcl (n.d.),hr W/m2C with stepwise iteration
    IREQ=0.5
    hr=3
    ArAdu=0.77
    factor=0.5 #Initial values !
    Balance=1
    W=1
    while (abs(Balance) > 0.01) {
      fcl=1+1.197*IREQ
      Rt=(0.06/0.38)*(Ia+IREQ)
      E=wetness*(Psks-Pa)/Rt
      Hres=1.73e-02*M*(Pex-Pa)+1.4e-03*M*(Tex-Ta)
      Tcl=Tsk-IREQ*(M-W-E-Hres)
      hr=5.67e-08*0.95*ArAdu*(exp(4*log(273+Tcl))-
                               exp(4*log(273+Tr)))/(Tcl-Tr)
      hc=1/Ia-hr
      R=fcl*hr*(Tcl-Tr)
      C=fcl*hc*(Tcl-Ta)
      Balance=M-W-E-Hres-R-C
      if (Balance>0)  {
        IREQ=IREQ-factor
        factor=factor/2
      }
      else {
        IREQ=IREQ+factor
      }
    }
    IREQ=(Tsk-Tcl)/(R+C)
    # *** Calculation of Dlimneutral and Dlimminimal ***
    # Calculation of S (W/m2), Rt (m2kPa/W), fcl (n.d.), hr W/m2C with stepwise iteration
    Tcl=Ta
    hr=3
    S=-40
    ArAdu=0.77
    factor=500
    Iclr=Icl # Initial values !
    Balance=1
    W=1
    DLE=1
    while (abs(Balance) > 0.01) {
      fcl=1+1.197*Iclr
      Iclr=((Icl+0.085/fcl)*(0.54*exp(-0.15*v-0.22*w)*(p^0.075)-0.06*log(p)+0.5)-
              (0.092*exp(-0.15*v-0.22*w)-0.0045)/fcl)
      Rt=(0.06/0.38)*(Ia+Iclr)
      E=wetness*(Psks-Pa)/Rt
      Hres=1.73e-02*M*(Pex-Pa)+1.4e-03*M*(Tex-Ta)
      Tcl=Tsk-Iclr*(M-W-E-Hres-S)
      hr=5.67e-08*0.95*ArAdu*(exp(4*log(273+Tcl))-
                               exp(4*log(273+Tr)))/(Tcl-Tr)
      hc=1/Ia-hr
      R=fcl*hr*(Tcl-Tr)
      C=fcl*hc*(Tcl-Ta)
      Balance=M-W-E-Hres-R-C-S
      if (Balance>0)  {
        S=S+factor
        factor=factor/2
      }
      else {
        S=S-factor
      }
    }
    DLE = -40/S
    IREQneutral =1
    ICLneutral =1
    DLEneutral =1
    IREQminimal =1
    ICLminimal =1
    DLEminimal =1
    if (calculation==1) {
      IREQminimal=round((IREQ/0.155)*10)/10
      ICLminimal=round((((IREQ+Ia/fcl)/(0.54*exp(-0.15*v-0.22*w)*
                                          (p^0.075)-0.06*log(p)+0.5))-0.085/fcl)/0.155*10)/10
      cat("\nIREQminimal value: ", IREQminimal)
      cat("\nICLminimal value: ", ICLminimal)
      if (S>-8){
        DLEminimal="more than 8"
        cat("\nDLEminimal value is: ", DLEminimal)
      }
      else{
        DLEminimal=round(DLE*10)/10
        cat("\nDLEminimal value: ", DLEminimal)
      }
    }
    else{
      IREQneutral=round((IREQ/0.155)*10)/10
      ICLneutral=round((((IREQ+Ia/fcl)/(0.54*exp(-0.15*v-0.22*w)*
                                          (p^0.075)-0.06*log(p)+0.5))-0.085/fcl)/0.155*10)/10
      cat("\nIREQneutral value: ", IREQneutral)
      cat("\nICLneutral value: ", ICLneutral)
      if (S>-8){
        DLEneutral="more than 8"
        cat("\nDLEneutral value is: ", DLEneutral)
      }
      else{
        DLEneutral=round(DLE*10)/10
        cat("\nDLEneutral value: ", DLEneutral)
      }
      if (Icl/0.155>ICLneutral){
        message="AVAILABLE > REQUIRED MINIMAL & NEUTRAL basic clothing insulation"
        print(message)
      }
    }
  }


}

calcIREQ(116,0,-15,-15,8,0.3,0.4,85,2.5)

