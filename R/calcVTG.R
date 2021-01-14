#' @title Calculate PPD with Vertical Air Temperature Gradient
#' @description Function to calculate vertical air temperature gradient using the function predicted percentage of dissatisfied.
#' @aliases  vairtepgrad
#' @aliases  vtg
#' @aliases  vairtmpgrad
#' @usage calcVTG(ta, tr, vel, rh, clo, met, v_tmp_grad)
#' @usage vtg(ta, tr, vel, rh, clo, met, v_tmp_grad)
#' @usage vairtmpgrad(ta, tr, vel, rh, clo, met, v_tmp_grad)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param v_tmp_grad vertical temperature gradient between the feet and the head [degree C/m]
#' @details  Calculates the percentage of thermally dissatisfied people with a vertical temperature gradient between feet and head.This equation is only applicable for velocity(vel) < 0.2 m/s (40 fps)
#' @returns Predicted Percentage of Dissatisfied occupants with vertical temperature gradient in [\%]
#' @returns Acceptability in [boolean]
#' @examples calcVTG(25,25,0.1,50,0.5,1.2,7)
#' @author Shoaib Sarwar
#' @export

calcVTG <- function(ta,tr,vel,rh,clo,met,v_tmp_grad){

  tsv = calcPMV(ta=ta, tr=tr, vel=vel, rh=rh, clo=clo, met=met)
  tsv = round(tsv,1)
  numerator = exp(0.13 * (tsv - 1.91) ** 2 + 0.15 * v_tmp_grad - 1.6)
  ppdval = (numerator / (1 + numerator) - 0.345) * 100


      ppdval = round(ppdval,1)

      acceptability = ppdval <= 5

      print(paste0("PMV value: ", tsv))

      print(paste0("Vertical Air Temperature Gradient: ", ppdval))

      print(paste0("Acceptability: ", acceptability))


      if (vel > 0.2)
      {

        warning('Velocity(vel) should be less than or equal to 0.2.')

      }





}
vtg <- calcVTG
vairtmpgrad <- calcVTG

