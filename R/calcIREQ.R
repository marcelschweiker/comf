#' @title Calculates IREQ and Dlim
#' @description Function to calculate minimal and neutral values of REQUIRED CLOTHING INSULATION (IREQ) and DURATION LIMITED EXPOSURE (Dlim).
#' @aliases  calcireq
#' @aliases  IREQ
#' @aliases  ireq
#' @usage calcIREQ(M,W,ta,tr,p,w,v,rh,clo)
#' @usage calcireq(M,W,ta,tr,p,w,v,rh,clo)
#' @usage IREQ(M,W,ta,tr,p,w,v,rh,clo)
#' @usage ireq(M,W,ta,tr,p,w,v,rh,clo)
#' @param M a numeric value presenting metabolic energy production (58 to 400 W/m2) in [W/m2]
#' @param W a numeric value presenting Rate of mechanical work, (normally 0) in [W/m2]
#' @param ta a numeric value presenting ambiant air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param p a numeric value presenting air permeability (low < 5, medium 50, high > 100 l/m2s) in [l/m2s]
#' @param w a numeric value presenting walking speed (or calculated work created air movements) in [m/s]
#' @param v a numeric value presenting relative air velocity(0.4 to 18 m/s) in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @details  The function gives IREQ Insulation of clothing required to maintain thermal equilibrium of the body at specified levels of physiological stress.
#' @returns \code{calcIREQ} returns
#' \item{IREQminimal }{Lower bound of insulation required in [clo]}
#' \item{IREQneutral }{Upper bound of insulation required in [clo]}
#' \item{ICLminimal }{Lower bound of REQUIRED basic clothing insulation (ISO 9920) in [clo]}
#' \item{ICLneutral }{Upper bound of REQUIRED basic clothing insulation (ISO 9920) in [clo]}
#' \item{DLEminimal }{Lower bound of duration limited exposure in [hours]}
#' \item{DLEneutral }{upper bound of duration limited exposure in [hours]}
#' @examples calcIREQ(116,0,-15,-15,8,0.3,0.4,85,2.5)
#' @examples calcireq(116,0,-15,-15,8,0.3,0.4,85,2.5)
#' @examples ireq(116,0,-15,-15,8,0.3,0.4,85,2.5)
#' @examples IREQ(116,0,-15,-15,8,0.3,0.4,85,2.5)
#' @references ISO 11079, 2007-12-15, ERGONOMICS OF THE THERMAL ENVIRONMENT - DETERMINATION AND INTERPRETATION OF COLD STRESS WHEN USING REQUIRED CLOTHING INSULATION (IREQ) AND LOCAL COOLING EFFECTS
#' @note The authors disclaim all obligations and liabilities for damages arising from the use or attempted use of the information, including, but not limited to, direct, indirect, special and consequential damages, and attorneys' and experts' fees and court costs. Any use of the information will be at the risk of the user.
#' @author Developed by Ingvar Holmer and Hakan O. Nilsson, 1990 and transferred to R by Shoaib Sarwar and Marcel Schweiker.
#' @export



calcIREQ <- function(M,W,ta,tr,p,w,v,rh,clo){
  S<-1

  if (M <= 58){
    M <- 58
  }
  if (M >= 400){
    M<-400
  }
  if (ta >= 10){
    ta<-10
  }
  # Calculation of stationary w (m/s)
  if (w <= 0.0052*(M-58)) {
    w<-0.0052*(M-58)
  }
  if (w >= 1.2) {
    w<-1.2
  }
  if (v <= 0.4) {
    v<-0.4
  }
  if (v >= 18) {
    v<-18
  }
  clo <- clo * 0.155
  Ia <-0.092*exp(-0.15 * v - 0.22 *  w) - 0.0045
  calculation<-0
  while(calculation<2){
    calculation<-calculation+1
    #Calculation of Tsk (C) and wetness (%)
    if (calculation==1) {
      #For IREQminimal, DLEminimal !
      Tsk<-33.34-0.0354*M
      wetness<-0.06
    }
    else  {
      #For IREQneutral, DLEneutral !
      Tsk<-35.7-0.0285*M
      wetness<-0.001*M
    }
    #Calculation of Tex (C) and Pex,Psks,Pa (Pa)
    Tex<-29+0.2*ta
    Pex<-0.1333*exp(18.6686-4030.183/(Tex+235))
    Psks<-0.1333*exp(18.6686-4030.183/(Tsk+235))
    Pa<-(rh/100)*0.1333*exp(18.6686-4030.183/(ta+235))
    #Calculation of IREQ (m2C/W),Rt (m2kPa/W),fcl (n.d.),hr W/m2C with stepwise iteration
    IREQ<-0.5
    hr<-3
    ArAdu<-0.77
    factor<-0.5 #Initial values !
    Balance<-1
    #W<-1
    while (abs(Balance) > 0.01) {
      fcl<-1+1.197*IREQ
      Rt<-(0.06/0.38)*(Ia+IREQ)
      E<-wetness*(Psks-Pa)/Rt
      Hres<-1.73e-02*M*(Pex-Pa)+1.4e-03*M*(Tex-ta)
      Tcl<-Tsk-IREQ*(M-W-E-Hres)
      hr<-5.67e-08*0.95*ArAdu*(exp(4*log(273+Tcl))-
                                exp(4*log(273+tr)))/(Tcl-tr)
      hc<-1/Ia-hr
      R<-fcl*hr*(Tcl-tr)
      C<-fcl*hc*(Tcl-ta)
      Balance<-M-W-E-Hres-R-C
      if (Balance>0)  {
        IREQ<-IREQ-factor
        factor<-factor/2
      }
      else {
        IREQ<-IREQ+factor
      }
    }
    IREQ=(Tsk-Tcl)/(R+C)
    # *** Calculation of Dlimneutral and Dlimminimal ***
    # Calculation of S (W/m2), Rt (m2kPa/W), fcl (n.d.), hr W/m2C with stepwise iteration
    Tcl<-ta
    hr<-3
    S<--40
    ArAdu<-0.77
    factor<-500
    Iclr<-clo # Initial values !
    Balance<-1
    #W<-1
    #DLE<-1
    while (abs(Balance) > 0.01) {
      fcl<-1+1.197*Iclr
      Iclr<-((clo+0.085/fcl)*(0.54*exp(-0.15*v-0.22*w)*(p^0.075)-0.06*log(p)+0.5)-
              (0.092*exp(-0.15*v-0.22*w)-0.0045)/fcl)
      Rt<-(0.06/0.38)*(Ia+Iclr)
      E<-wetness*(Psks-Pa)/Rt
      Hres<-1.73e-02*M*(Pex-Pa)+1.4e-03*M*(Tex-ta)
      Tcl<-Tsk-Iclr*(M-W-E-Hres-S)
      hr<-5.67e-08*0.95*ArAdu*(exp(4*log(273+Tcl))-
                                exp(4*log(273+tr)))/(Tcl-tr)
      hc<-1/Ia-hr
      R<-fcl*hr*(Tcl-tr)
      C<-fcl*hc*(Tcl-ta)
      Balance<-M-W-E-Hres-R-C-S
      if (Balance>0)  {
        S<-S+factor
        factor<-factor/2
      }
      else {
        S<-S-factor
      }
    }
    DLE <- -40/S
    if (calculation==1) {
      IREQminimal<-round((IREQ/0.155)*10)/10
      ICLminimal<-round((((IREQ+Ia/fcl)/(0.54*exp(-0.15*v-0.22*w)*
                                          (p^0.075)-0.06*log(p)+0.5))-0.085/fcl)/0.155*10)/10
      #cat("\nIREQminimal value: ", IREQminimal)
      #cat("\nICLminimal value: ", ICLminimal)
      if (S>-8){
        DLEminimal<-"more than 8"
        #cat("\nDLEminimal value is: ", DLEminimal)
      }
      else{
        DLEminimal<-round(DLE*10)/10
        #cat("\nDLEminimal value: ", DLEminimal)
      }
    }
    else{
      IREQneutral<-round((IREQ/0.155)*10)/10
      ICLneutral<-round((((IREQ+Ia/fcl)/(0.54*exp(-0.15*v-0.22*w)*
                                          (p^0.075)-0.06*log(p)+0.5))-0.085/fcl)/0.155*10)/10
      #cat("\nIREQneutral value: ", IREQneutral)
      #cat("\nICLneutral value: ", ICLneutral)
      if (S>-8){
        DLEneutral<-"more than 8"
        #cat("\nDLEneutral value is: ", DLEneutral)
      }
      else{
        DLEneutral<-round(DLE*10)/10
        #cat("\nDLEneutral value: ", DLEneutral)
      }
      if (clo/0.155>ICLneutral){
        message<-"AVAILABLE > REQUIRED MINIMAL & NEUTRAL basic clothing insulation"
        print(message)
      }

    }

  }

  data.frame(IREQminimal,IREQneutral, ICLminimal,ICLneutral,DLEminimal,DLEneutral)
}

IREQ <- calcIREQ
ireq <- calcIREQ
calcireq<- calcIREQ

