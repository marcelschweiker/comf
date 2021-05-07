#' @title Calculate areteriovenous anastmoses (AVA) blood flow rate [L/h]
#' @description Function to Calculate areteriovenous anastmoses (AVA) blood flow rate [L/h] based on Takemori's model, 1995.
#' @aliases calcEvap
#' @usage calcEvaporation(err_cr, err_sk, tsk, ta, rh, ret, height=1.72, weight=74.43, equation="dubois", age=20)
#' @usage calcEvap(err_cr, err_sk, tsk, ta, rh, ret, height=1.72, weight=74.43, equation="dubois", age=20)
#' @param err_cr Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param err_sk Float or array, optional. Difference between setpoint and body temperatures.The default is 0.
#' @param tsk Skin temperatures in [Degree Celcius]
#' @param ta Air temperatures at local body segments in [Degree Celcius]
#' @param rh  Relative humidity at local body segments [%]
#' @param ret Total evaporative thermal resistances [m2.K/W]
#' @param height Body height [m]. The default is 1.72.
#' @param weight Body weight [kg]. The default is 74.43.
#' @param equation
#' @param age Age [years]. The default is 20.
#' @returns  BFava_hand: AVA blood flow rate at hand [L/h].
#' @returns BFava_foot : AVA blood flow rate at foot [L/h].
#' @example
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
calcAVAbloodflow <- function(err_cr, err_sk,
                  height=1.72, weight=74.43, equation="dubois", age=20, ci=2.59){

# Cal. mean error body core temp.
cap_bcr = c(10.2975, 9.3935, 13.834)  # Thermal capacity at Chest, Back and Pelvis
err_bcr = weighted.mean(err_cr[3:5], weights=cap_bcr)

#Cal. mean error skin temp.
bsa = BSAst
err_msk =weighted.mean(err_sk, weights=bsa)

# Openbess of AVA [-]
sig_ava_hand = 0.265 * (err_bcr + 0.43) + 0.953 * (err_msk + 0.1905) + 0.9126
sig_ava_foot = 0.265 * (err_bcr - 0.97) + 0.953 * (err_msk - 0.0095) + 0.9126

sig_ava_hand = min(sig_ava_hand, 1)
sig_ava_hand = max(sig_ava_hand, 0)
sig_ava_foot = min(sig_ava_foot, 1)
sig_ava_foot = max(sig_ava_foot, 0)

bfbr = calcBFBrate(height, weight, equation, age, ci)
# AVA blood flow rate [L/h]
bf_ava_hand = 1.71 * bfbr * sig_ava_hand  # Hand
bf_ava_foot = 2.16 * bfbr * sig_ava_foot  # Foot
return(list(blooflowfoot = bf_ava_foot,blooflowhand= bf_ava_hand))
}

