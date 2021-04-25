#' @title Calculate Calculate local metabolic rate by non-shivering [W]
#' @description Function to Calculate local metabolic rate by non-shivering [W]
#' @aliases calcnoshiver
#' @usage calcNOshivering(err_cr, err_sk, tcr, tsk,height=1.72, weight=74.43, equation="dubois", age=20, sex="male", dtime=60,options)
#' @param err_cr Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param err_sk Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param tsk Skin temperatures in [Degree Celcius]
#' @param tcr core temperatures at local body segments in [Degree Celcius]
#' @param height Body height [m]. The default is 1.72.
#' @param weight Body weight [kg]. The default is 74.43.
#' @param equation helpjos
#' @param age Age [years]. The default is 20.
#' @param sex [str] optional.Choose male or female. The default is "male".
#' @param dtime float, optional Interval of analysis time. The default is 60.
#' @param options see helpjos
#' @returns  Mshiv : array Local metabolic rate by non-shivering [W].
#' @example
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
calcNOshivering <- function(err_cr, err_sk,
                 height=1.72, weight=74.43, equation="dubois", age=20,
                 coldacclimation= FALSE, batpositive= TRUE
                 #options
                ){
# NST (Non-Shivering Thermogenesis) model, Asaka, 2016
  error <- calcERRORsig(err_cr, err_sk)  # Thermoregulation signals
  wrms <- error$wrms
  clds <- error$clds

bmi <- weight / height^2

# BAT: brown adipose tissue [SUV]
bat = 10^(-0.10502 * bmi + 2.7708)

# age factor
if (age < 30){
  bat <- bat * 1.61}
else if (age < 40){
  bat <- bat * 1.00}
else { # age >= 40
  bat <- bat * 0.80}

if (coldacclimation){
  bat <- bat + 3.46}

if (batpositive == FALSE){

  if (age < 30){
  bat <- bat * 44/83
  }
else if (age < 40){
  bat<- bat * 15/38}
  else if (age < 50){
  bat <- bat *  7/26}
  else if (age < 50){
  bat <- bat *  1/8}
  else{ # age > 60
  bat <- bat *  0}
}
# NST limit
thres = ((1.80 * bat + 2.43) + 5.62)  # [W]
sig_nst = 2.8 * clds  # [W]
sig_nst =  min(sig_nst, thres)

mnstf = c(
  0.000, 0.190, 0.000, 0.190, 0.190,
  0.215, 0.000, 0.000, 0.215, 0.000, 0.000,
  0.000, 0.000, 0.000, 0.000, 0.000, 0.000)
bsar = calcBSArate(height, weight, equation)
mnst = bsar * mnstf * sig_nst
return (list(mnst =mnst))
}

