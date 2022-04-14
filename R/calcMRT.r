#initialize constants

#Prandtl constants
cp <- 1005 # J/kg/K
kAir <- 0.02662 # W/m-K
mu <- 0.0000181 # Pa s

#Raleigh constants
g <- 9.81 # m/s2
beta <- 0.0034 # 1/K
nu <- 0.0000148 # m2/s
alpha <- 0.00002591 # m2/s

#Globe diameter [m]
x <- 0.150
## This model has only been validated from x = 0.040m (ping pong ball) to
## x = 0.150m (standard globe thermometer) globes

Pr <- cp*mu/kAir

o <- 0.0000000567
emiss <- 0.95

n <- 1.27*x+0.57

calcMRT <- function(tg, ta, va){
  Ra <- g*beta*abs(tg-ta)*x*x*x/nu/alpha
  Re <- va*x/nu
  
  nuNatural <- 2 + (0.589*(Ra ^(1/4))) / (1+((0.469/Pr) ^ (9/16))) ^ (4/9)
  nuForced <- 2+(0.4*(Re ^ 0.5) + 0.06*(Re ^ 2/3))*(Pr ^ 0.4)
  
  mixedCorrection <- ((((tg+273.15) ^ 4)-((((((nuForced ^ n)+(nuNatural ^ n)) ^ (1/n))*kAir/x)*(-tg+ta))/emiss/o)) ^ 0.25) - 273.15
  standardCorrection  <- ((((tg+273.15) ^ 4)-(((1.1*100000000*(va ^ 0.6)*(-tg+ta))/emiss/(x ^ 0.4)))) ^ 0.25) - 273.15
  
  print(standardCorrection)
  
  print(paste("MRT calculated with Mixed Convection = ",as.character(round(mixedCorrection,1))," oC"))
  print(paste("MRT calculated with only Forced Convection (ASHRAE) = ",as.character(round(standardCorrection,1))," oC"))
  print(paste("The two methods differ by ",as.character(round(abs(mixedCorrection-standardCorrection),1))," K"))
  c(mixedCorrection, standardCorrection)
}

#Globe temperature [C]
TGlobe <- 30

#Air temperature [C]
TAir <- 24

#Air speed [m/s]
vAir <- 0.0

calcMRT(TGlobe, TAir, vAir)

