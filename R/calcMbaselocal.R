#' @title Calculate local basal metabolic rate [W].
#' @description Function to Calculate local basal metabolic rate [W].
#' @param height Body height. The default is 1.72 in [m]
#' @param weight Body height. The default is 74.43 in [kg]
#' @param age Age. The default is 20 in [years]
#' @param sex  optional. Choose male or female. The default is "male" in [str]
#' @param equation (Optional)Choose harris-benedict or ganpule. The default is "harris-benedict" in [str]
#' @details Choose a name from "dubois","takahira", "fujimoto", or "kurazumi". All these equations are defined in helpJOS functions.
#' @returns  mbase : array Local basal metabolic rate (Mbase) [W]
#' @author Code implemented in to R by Shoaib Sarwar. Further contribution by Marcel Schweiker.
#' @seealso \code{\link{helpJOS}}
#' @export

calcMbaselocal <- function(height=1.72, weight=74.43, age=20,
                sex="male", equation="harris-benedict"){

mbase_all <- calcMETbasal(height, weight, age, sex, equation)
# Distribution coefficient of basal metabolic rate
mbf_cr <- c(
  0.19551, 0.00324, 0.28689, 0.25677, 0.09509,
  0.01435, 0.00409, 0.00106, 0.01435, 0.00409, 0.00106,
  0.01557, 0.00422, 0.00250, 0.01557, 0.00422, 0.00250)
mbf_ms <- c(
  0.00252, 0.0, 0.0, 0.0, 0.04804,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
mbf_fat <- c(
  0.00127, 0.0, 0.0, 0.0, 0.00950,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
mbf_sk <- c(
  0.00152, 0.00033, 0.00211, 0.00187, 0.00300,
  0.00059, 0.00031, 0.00059, 0.00059, 0.00031, 0.00059,
  0.00144, 0.00027, 0.00118, 0.00144, 0.00027, 0.00118)

mbase_cr <- mbf_cr * mbase_all
mbase_ms <- mbf_ms * mbase_all
mbase_fat <- mbf_fat * mbase_all
mbase_sk <- mbf_sk * mbase_all
return(list(mbase_cr=mbase_cr, mbase_ms=mbase_ms, mbase_fat=mbase_fat, mbase_sk=mbase_sk))
}

