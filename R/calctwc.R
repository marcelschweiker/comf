calctwc <- function(v,Tawci){

  v = v*1.0
  twc = 13.12 + 0.6215 * Tawci - 11.37 * (v^0.16) + 0.3965 * Tawci * (v^0.16)
  twc = round(twc)
  return(twc)
}

calctwc(20,35)
