#' @title Calculate basal metabolic rate [W]
#' @description Function to Calculate Calculate basal metabolic rate [W]
#' @param height Body height. The default is 1.72 in [m]
#' @param weight Body height. The default is 74.43 in [kg]
#' @param age Age. The default is 20 in [years]
#' @param sex  optional. Choose male or female. The default is "male" in [str]
#' @param equation (Optional)Choose harris-benedict or ganpule. The default is "harris-benedict" in [str]
#' @details Choose a name from "dubois","takahira", "fujimoto", or "kurazumi". All these equations are defined in helpJOS functions.
#' @returns  BMR : float Basal metabolic rate in [W]
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export
calcMETbasal <- function(height=1.72, weight=74.43, age=20,
              sex="male", equation="harris-benedict"){

  BMR = switch(
    equation,
    "harris-benedict" = {
      if(sex=="male") 88.362 + 13.397*weight + 500.3*height - 5.677*age 
      else 447.593 + 9.247*weight + 479.9*height - 4.330*age
    },
    "harris-benedict_origin" = {
      if(sex=="male") 66.4730 + 13.7516*weight + 500.33*height - 6.7550*age 
      else 655.0955 + 9.5634*weight + 184.96*height - 4.6756*age},
    {
      if(sex=="male") (0.0481*weight + 2.34*height - 0.0138*age - 0.4235) * 238.8915
      else (0.0481*weight + 2.34*height - 0.0138*age - 0.9708) * 238.8915}
  )
  BMR * 0.048
}
