#' @title PPD with Ankle Draft
#' @description Function to calculate ankle draft using the predicted percentage of dissatisfied.
#' @aliases ankledraft
#' @aliases ad
#' @usage calcAD(ta, tr, vel, rh, clo, met, vAnkle)
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param vAnkle air speed at the 0.1 m (4 in.) above the floor [m/s]
#' @details  Calculates the percentage of thermally dissatisfied people with the ankle draft (0.1 m) above floor level.This equation is only applicable for velocity < 0.2 m/s (40 fps)
#' @returns Predicted Percentage of Dissatisfied occupants with ankle draft in [\%]
#' @returns Acceptability in [boolean]
#' @references 
#' Original code in Python by Tartarini & Schiavon (2020) <doi:10.1016/j.softx.2020.100578>
#' @examples calcAD(25,25,0.2,50,0.5,1.2,0.3) # returns Ankle_draft_ppd:18.6, Acceptability:TRUE
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker. 
#' @export



calcAD <- function(ta,tr,vel,rh,clo,met,vAnkle){



  tsv = calcPMV(ta=ta, tr=tr, vel=vel, rh=rh, clo=clo, met=met)

  ppdval = (exp(-2.58 + 3.05 * vAnkle - 1.06 * tsv)/(1 + exp(-2.58 + 3.05 * vAnkle -1.06 * tsv))) * 100


     ppdval = round(ppdval,1)

     acceptability = ppdval <= 20

     print(paste0("PMV value: ", round(tsv,3)))

     print(paste0("Ankle_draft_ppd: ", ppdval))

     print(paste0("Acceptability: ", acceptability))

     if (vel > 0.2)
       {

       warning('Velocity(vel) should be less than or equal to 0.2')

     }

}
