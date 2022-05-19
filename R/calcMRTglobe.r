#' MRT calculation based on standard and mixed convection
#' 
#' aliases MRT globe
#' @description \code{calcMRTglobe} calculates the mean radiant temperature considering mixed convection
#' 
#' @usage calcMRTglobe(tg, ta, vel, x = 0.15)
#'
#' @param tg - a numeric value presenting globe temperature in [degree C]
#' @param ta - a numeric value presenting air temperature in [degree C]
#' @param vel - a numeric value presenting air velocity in [m/s]
#' @param x - a numeric value presenting globe diameter in [m]
#'
#' @return \code{calcMRTglobe} MRT with standard and mixed correction
#' 
#' @details This model has only been validated from x = 0.040m (ping pong ball) to
#' @details x = 0.150m (standard globe thermometer) globes
#' 
#' @references 
#' Teitelbaum et al. (2022) <10.1038/s41598-022-10172-5>
#' Teitelbaum (2022)  <https://github.com/eteitelb/MixedConvection>
#'
#' @seealso see also \code{\link{calcMRT}}
#'
#' @author code implemented into R by Shaomi Rahman and Marcel Schweiker.
#' @export
#'
#' @examples #Globe temperature [C]
#' @examples tg <- 30
#' @examples #Air temperature [C]
#' @examples ta <- 24
#' @examples #Air speed [m/s]
#' @examples vel <- 0.0
#' @examples calcMRTglobe(tg, ta, vel)


calcMRTglobe <- function(tg, ta, vel, x = 0.15){
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

	Pr <- cp*mu/kAir

	o <- 0.0000000567
	emiss <- 0.95

	n <- 1.27*x+0.57

  Ra <- g*beta*abs(tg-ta)*x*x*x/nu/alpha
  Re <- vel*x/nu
  
  nuNatural <- 2 + (0.589*(Ra ^(1/4))) / (1+((0.469/Pr) ^ (9/16))) ^ (4/9)
  nuForced <- 2+(0.4*(Re ^ 0.5) + 0.06*(Re ^ 2/3))*(Pr ^ 0.4)
  
  mixedCorrection <- ((((tg+273.15) ^ 4)-((((((nuForced ^ n)+(nuNatural ^ n)) ^ (1/n))*kAir/x)*(-tg+ta))/emiss/o)) ^ 0.25) - 273.15
  standardCorrection  <- ((((tg+273.15) ^ 4)-(((1.1*100000000*(vel ^ 0.6)*(-tg+ta))/emiss/(x ^ 0.4)))) ^ 0.25) - 273.15
  
  #print(standardCorrection)
  
  print(paste("MRT calculated with Mixed Convection = ",as.character(round(mixedCorrection,1))," oC"))
  print(paste("MRT calculated with only Forced Convection (ASHRAE) = ",as.character(round(standardCorrection,1))," oC"))
  print(paste("The two methods differ by ",as.character(round(abs(mixedCorrection-standardCorrection),1))," K"))
  c(mixedCorrection, standardCorrection)
}

