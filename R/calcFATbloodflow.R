#' @title Calculate core, muslce and fat blood flow rate [L/h]..
#' @description Function to Calculate core, muslce and fat blood flow rate [L/h].
#' @param mwork array of Metablic rate by work [W].
#' @param mshiv array of Metablic rate by shivering [W].
#' @param height Body height [m]. The default is 1.72.
#' @param weight Body weight [kg]. The default is 74.43.
#' @param equation str, optional. The equation name (str) of bsa calculation. Choose a name from "dubois","takahira", "fujimoto", or "kurazumi". The default is "dubois".
#' @param age The default is 20 in [years].
#' @param ci  float, optional Cardiac index [L/min/㎡]. The default is 2.59.
#' @returns  BFcr, BFms, BFfat : array Core, muslce and fat blood flow rate [L/h].
#' @example
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
calcFATbloodflow <- function(mwork, mshiv, height=1.72, weight=74.43, 
                             equation="dubois", age=20, ci=2.59, idict){
# Basal blood flow rate [L/h]
# core, CBFB
bfb_cr = c(
  35.251, 15.240, 89.214, 87.663, 18.686,
  1.808, 0.940, 0.217, 1.808, 0.940, 0.217,
  1.406, 0.164, 0.080, 1.406, 0.164, 0.080)
# muscle, MSBFB
bfb_ms = c(
  0.682, 0.0, 0.0, 0.0, 12.614,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
# fat, FTBFB
bfb_fat = c(
  0.265, 0.0, 0.0, 0.0, 2.219,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

bfbr = calcBFBrate(height, weight, equation, age, ci)
bf_cr = bfb_cr * bfbr
bf_ms = bfb_ms * bfbr
bf_fat = bfb_fat * bfbr

for(i  in 1:length(bodyNames)){
  bn <- bodyNames[i]
  if (!is.na( idict[[bn]][["muscle"]])){
    bf_ms[i] = bf_ms[i]  + (mwork[i] + mshiv$mshiv[i])/1.163
  }
  else {
    bf_cr[i] = bf_cr[i] + (mwork[i] + mshiv$mshiv[i])/1.163
  }
}
return(list(bf_cr=bf_cr, bf_ms=bf_ms, bf_fat=bf_fat))
}


