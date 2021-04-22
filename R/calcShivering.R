#' @title Calculate local metabolic rate by shivering [W].
#' @description Function to calculate Calculate local metabolic rate by shivering [W].
#' @aliases calcshiver
#' @usage calcShivering(err_cr, err_sk, tcr, tsk,height=1.72, weight=74.43, equation="dubois", age=20, sex="male", dtime=60,options)
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
#' @returns  Mshiv : array Local metabolic rate by shivering [W].
#' @example
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
calcShivering <- function(err_cr, err_sk, tcr, tsk,
              height=1.72, weight=74.43, equation="dubois", age=20, sex="male", dtime=60,
              options){

  PRE_SHIV = 0
  error <- calcERRORsig(err_cr, err_sk)  # Thermoregulation signals
  wrms <- error$wrms
  clds <- error$clds
  shivf = c(
  0.0339, 0.0436, 0.27394, 0.24102, 0.38754,
  0.00243, 0.00137, 0.0002, 0.00243, 0.00137, 0.0002,
  0.0039, 0.00175, 0.00035, 0.0039, 0.00175, 0.00035)
sig_shiv = 24.36 * clds * (-err_cr[1])
sig_shiv = max(sig_shiv, 0)


  if (options$limit_dshivdt){
    tskm = weighted.mean(tsk, BSAst())# Mean skin temp.
    # Asaka, 2016
    # Threshold of starting shivering
    if (tskm <31){
      thres =36.6}
    else {
      if (sex == "male"){
      thres = -0.2436 * tskm + 44.10
    }
    else if (sex == "female")
    {thres = -0.2250 * tskm + 43.05
      }
    }
    # Second threshold of starting shivering
    if (thres < tcr[1]){
      sig_shiv = 0}
  }

  if (options$limit_dshivdt == TRUE){
  # Asaka, 2016
  # dshiv < 0.0077 [W/s]
  dshiv = sig_shiv - PRE_SHIV
     if (options$limit_dshivdt == TRUE){ # default is 0.0077 [W/s]
  limit_dshiv = 0.0077 * dtime}
     else
 {limit_dshiv = (options$limit_dshivdt) * dtime}

     if (dshiv > limit_dshiv){
  sig_shiv = limit_dshiv + PRE_SHIV}
     else if (dshiv < -limit_dshiv){
  sig_shiv = -limit_dshiv + PRE_SHIV}
}
PRE_SHIV = sig_shiv

# Signal sd_shiv by aging
if (age < 30)
  {sd_shiv = rep(1,17)}
else if (age < 40)
  {sd_shiv = rep(1,17) * 0.97514}
else if (age < 50)
  {sd_shiv = rep(1,17) * 0.95028}
else if (age < 60)
  {sd_shiv = rep(1,17) * 0.92818}
else if (age < 70)
  {sd_shiv = rep(1,17) * 0.90055}
else if (age < 80)
  {sd_shiv = rep(1,17) * 0.86188}
else #age >= 80
  {sd_shiv = rep(1,17) * 0.82597}

bsar = calcBSArate(height, weight, equation)
mshiv = shivf * bsar * sd_shiv * sig_shiv
return (list(mshiv =mshiv))

}

