#' @title Thermal Comfort Indices using a List of Climatic Conditions
#' @description \code{calcComfInd} calculates one or more thermal comfort indices using a list of climatic conditions.
#' @aliases comfind
#' @usage calcComfInd(lsCond, request = "all")
#' @usage comfind(lsCond, request = "all")
#' @param lsCond a list of climatic conditions and additional variables necessary for one or more of the indices (see details below).
#' @param request a vector with one or more comfort indices (see details below).
#' @details
#' The list \code{lsCond} could contain one or more of the following variables:
#' \tabular{ll}{
#'  ta    	\tab Air temperature in (degree C)  \cr
#'  tr    	\tab mean radiant temperature in (degree C) \cr
#'  vel   	\tab Air velocity in (m/s) \cr
#'  rh    	\tab Relative Humidity (\%) \cr
#'  clo   	\tab clothing (clo) \cr
#'  met   	\tab metabolic rate (met) \cr
#'  wme   	\tab External work (met) \cr
#'  tu    	\tab turbulence intensity (\%) \cr
#'  tmmo  	\tab mean monthly outdoor temperature in (degree C) \cr
#'  ltime 	\tab Exposure time (min) \cr
#'  pb    	\tab Barometric pressure (torr) \cr
#'  wt    	\tab weight (kg) \cr
#'  ht    	\tab height (cm) \cr
#'  trm   	\tab Running mean outdoor temperature in (degree C) \cr
#'  age	  	\tab age (years) \cr
#'  gender 	\tab gender (female = 1) \cr
#'  tsk   	\tab mean skin temperature in (degree C) \cr
#'  psych 	\tab factor related to fixed effect on perceived control \cr
#'  apCoeff \tab adaptive coefficient for pmv \cr
#'  epCoeff \tab expectancy factor for pmv \cr
#'  asCoeff \tab adaptive coefficient for set \cr
#'  esCoeff \tab expectancy factor for set \cr
#'  asv     \tab actual sensation vote (0 = neutral) \cr
#'  tao		\tab outdoor air temperature \cr
#'  rho		\tab outdoor relative humidity \cr
#'  frad 	\tab 0.7(for seating), 0.73(for standing) [-]  \cr
#'  eps 	\tab emissivity [-] \cr
#'  ic 	    \tab 1.084 (average permeability), 0.4 (low permeability)  \cr
#'  tcr		\tab initial values for core temp \cr
#'  tsk		\tab initial values for skin temperature \cr
#'  basMet	\tab basal metabolic rate \cr
#'  warmUp	\tab length of warm up period, i.e. number of times, loop is running for HBx calculation \cr
#'  cdil	\tab value for cdil in 2-node model of Gagge (applied in calculation of HbEx) \cr
#'  sigmatr \tab value for cdil in 2-node model of Gagge (applied in calculation of HbEx)
#' In case a variable is not given, but necessary for the respective index, a standard value from a list of values is used.
#' }
#'The vector \code{request} can contain the following elements:
#' \tabular{lll}{
#'  Element \tab Description \tab Required variables \cr
#'  "all" \tab Calculation of all indices described below \tab all variables \cr
#'  "pmv" \tab Predicted mean vote \tab  ta, tr, vel, rh, clo, met, wme \cr
#'  "ppd" \tab Predicted precentage dissatisfied \tab ta, tr, vel, rh, clo, met, wme \cr
#'  "tnhumphreys" \tab Neutral temperature according to Humphreys \tab tmmo \cr
#'  "tAdapt15251" \tab Adaptive comfort temperature according to EN 15251 \tab trm \cr
#'  "dTNZ" \tab Distance to thermoneutral zone \tab ht, wt, age, gender, clo, vel, tsk, ta \cr
#'  "ATHBpmv" \tab Adaptive thermal heat balance vote based on pmv \tab ta, tr, vel, rh, met, wme, psych, trm \cr
#'  "ATHBset" \tab Adaptive standard effective temperature \tab ta, tr, vel, rh, trm, met, wme, pb, ltime, ht, wt, psych \cr
#'  "ATHBpts" \tab Adaptive thermal heat balance vote based on set \tab ta, tr, vel, rh, trm, met, wme, pb, ltime, ht, wt, psych \cr
#'  "apmv" \tab Adaptive predicted mean vote according to Yao et al. \tab ta, tr, vel, rh, clo, met, wme, apCoeff \cr
#'  "ptsa" \tab Adaptive predicted thermal sensation vote according to Gao et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, asCoeff \cr
#'  "epmv" \tab pmv adjusted with expectancy factor based on Fanger and toftum \tab ta, tr, vel, rh, clo, met, wme, epCoeff, asv \cr
#'  "ptse" \tab Predicted thermal sensation vote based on set and adjusted with expectancy factor according to Gao et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, esCoeff, asv \cr
#'  "set" \tab standard effective temperature based on two node model by Gagge et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
#'  "et" \tab Effective temperature based on two node model by Gagge et al. \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
#'  "tsens" \tab Predicted thermal sensation \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
#'  "disc" \tab Predicted discomfort \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
#'  "ps" \tab Predicted percentage satisfied with the level of air movement \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
#'  "pd" \tab Predicted percentage dissatisfied due to draft \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt, tu \cr
#'  "pts" \tab Predicted thermal sensation vote based on set \tab ta, tr, vel, rh, clo, met, wme, pb, ltime, ht, wt \cr
#'  "HBxst" \tab Human body exergy consumPtion rate using steady state method \tab ta, tr, vel, rh, clo, met, tao, rho, frad, eps, ic, ht, wt, tcr, tsk, basMet, warmUp, cdil, sigmatr
#' }
#' @returns
#' \code{calcComfInd} returns one or more rows with the comfort indices listed as \code{request}. For details see details above.
#' @examples
#' ## Creating list with all values
#' lsCond <- createCond()
#' ## Requesting all comfort indices
#' calcComfInd(lsCond, request="all")
#' ## Requesting a single index
#' calcComfInd(lsCond, request="pmv")
#' ## Requesting multiple indices
#' calcComfInd(lsCond, request=c("pmv", "ptse"))
#' @author Sophia Mueller and Marcel Schweiker. Further contribution by Shaomi Rahman.
#' @seealso see also \code{\link{calcPMVPPD}}, \code{\link{calc2Node}}, \code{\link{calcHbExSteady}}, \code{\link{calcATHBpmv2015}}, \code{\link{calcdTNZ}}, \code{\link{calcPMVadj}}, \code{\link{calcPtsa}}, \code{\link{calctAdapt}}
#' @references For references see individual functions.
#' @note In case one of the variables is not given, a standard value will be taken from a list (see \code{\link{createCond}} for details.
#' @export


calcComfInd <- function(lsCond, request="all"){
  
  #if(is.data.frame(lsCond)){
  # lsCond <- as.list(lsCond)
  #}
  
  requests = c("pmv", "ppd", "tnHumphreysNV", "tnHumphreysAC", "tAdapt15251", "dTNZ", "dTNZTa", "dTNZts", "ATHBpmv", "ATHBset", "ATHBpts", "apmv", "ptsa", "epmv", "ptse", "epCoeff", "apCoeff", "esCoeff", "asCoeff", "set", "et", "tsens", "disc", "pd", "ps", "pts", "HBxst", "pmvadj", "humidex")
  
  # (1)
  met <- as.numeric(lsCond$met)
  wme <- as.numeric(lsCond$wme)
  pb <- as.numeric(lsCond$pb)
  ltime <- as.numeric(lsCond$ltime)
  rh <- as.numeric(lsCond$rh)
  clo <- as.numeric(lsCond$clo)
  ta <- as.numeric(lsCond$ta)
  tr <- as.numeric(lsCond$tr)
  tu <- as.numeric(lsCond$tu)
  vel <- as.numeric(lsCond$vel)
  ht <- as.numeric(lsCond$ht)
  wt <- as.numeric(lsCond$wt)
  tmmo <- as.numeric(lsCond$tmmo)
  trm <- as.numeric(lsCond$trm)
  age <- as.numeric(lsCond$age)
  gender <- as.numeric(lsCond$gender) # gender: 1=female; 2=male
  tsk <- as.numeric(lsCond$tsk)
  psych <- as.numeric(lsCond$psych)
  apCoeff <- as.numeric(lsCond$apCoeff)
  epCoeff <- as.numeric(lsCond$epCoeff)
  asCoeff <- as.numeric(lsCond$asCoeff)
  esCoeff <- as.numeric(lsCond$esCoeff)
  asv <- as.numeric(lsCond$asv)
  tao <- as.numeric(lsCond$tao)
  rho <- as.numeric(lsCond$rho)
  frad <- as.numeric(lsCond$frad)
  eps <- as.numeric(lsCond$eps)
  ic <- as.numeric(lsCond$ic)
  tcrI <- as.numeric(lsCond$tcrI)
  tskI <- as.numeric(lsCond$tskI)
  basMet <- as.numeric(lsCond$basMet)
  warmUp <- as.numeric(lsCond$warmUp)
  cdil <- as.numeric(lsCond$cdil)
  sigmatr <- as.numeric(lsCond$sigmatr)
  
  # (2)
  l<-max(c(1, length(met), length(wme), length(pb), length(ltime), length(rh), length(clo), length(ta), length(tr), length(vel), length(ht), length(wt), length(tmmo), length(trm), length(tu), length(age), length(gender), length(vel), length(tsk), length(psych), length(apCoeff), length(epCoeff), length(asCoeff), length(esCoeff), length(asv), length(tao), length(rho), length(frad), length(eps), length(ic), length(tcrI), length(tskI), length(basMet), length(warmUp), length(cdil), length(sigmatr)))
  
  namesLsCond <- names(lsCond)
  
  # (3)
  paramsAll <- c("met", "wme", "pb", "ltime", "rh", "clo", "ta", "tr", "vel", "ht", "wt", "tmmo", "trm", "tu", "age", "gender", "tsk", "psych", "apCoeff", "epCoeff", "asCoeff", "esCoeff", "asv", "tao", "rho", "frad", "eps", "ic", "tcrI", "tskI", "basMet", "warmUp", "cdil", "sigmatr")
  valsAll <-c(1, 0, 760, 60, 50, 0.5, 25, 25, 0.1, 170, 70, 15, 15, 40, 21, 1, 35, (-1.4), .293, .9, (-.2), 1.3, 1.5, 5, 70, .7, .95, 1.085, 37, 36, 58.2, 60, 100, .25)
  
  # creating list of necessary input values to check for consistency and missing values
  if (length(request==1) & request[1] == "all"){
    params <- paramsAll
    vals <- valsAll
  } else {
    params <- NULL
    # (4)
    for (nparam in 1:length(request)){
      if (request[nparam] == "pmv" | request[nparam] == "ppd"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme")
      } else if (request[nparam] == "tnHumphreysNV" | request[nparam] == "tnHumphreysAC"){
        paramsI <- c("tmmo")
      } else if (request[nparam] == "tAdapt15251"){
        paramsI <- c("trm")
      } else if (request[nparam] == "dTNZ" | request[nparam] == "dTNZTa" | request[nparam] == "dTNZTs"){
        paramsI <- c("ht", "wt", "age", "gender", "clo", "vel", "tsk", "ta", "met", "rh")
      } else if (request[nparam] == "ATHBpmv"){
        paramsI <- c("ta", "tr", "vel", "rh", "met", "wme", "psych", "trm")
      } else if (request[nparam] == "ATHBset" | request[nparam] == "ATHBpts"){
        paramsI <- c("ta", "tr", "vel", "rh", "trm", "met", "wme", "pb", "ltime", "ht", "wt", "psych")
      } else if (request[nparam] == "apmv"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "apCoeff")
      } else if (request[nparam] == "ptsa"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "pb", "ltime", "ht", "wt", "asCoeff")
      } else if (request[nparam] == "epmv"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "epCoeff")
      } else if (request[nparam] == "ptse"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "pb", "ltime", "ht", "wt", "esCoeff")
      } else if (request[nparam] == "epCoeff" | request[nparam] == "apCoeff"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "asv")
      } else if (request[nparam] == "esCoeff" | request[nparam] == "asCoeff"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "pb", "ltime", "ht", "wt", "asv")
      } else if (request[nparam] == "pd"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "pb", "ltime", "ht", "wt", "tu")
      } else if (request[nparam] == "pmvadj"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme")
      } else if (request[nparam] == "HBxst"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "tao", "rho", "frad", "eps", "ic", "ht", "wt", "tcrI", "tskI", "basMet", "warmUp", "cdil", "sigmatr")
      } else if (request[nparam] == "humidex"){
        paramsI <- c("ta", "rh")
      } else if (request[nparam] == "set" | request[nparam] == "et" | request[nparam] == "tsens" | request[nparam] == "disc" | request[nparam] == "ps" | request[nparam] == "pts"){
        paramsI <- c("ta", "tr", "vel", "rh", "clo", "met", "wme", "pb", "ltime", "ht", "wt")
        
      } else {
        stop(paste("error: ", request[nparam], " is not a valid request! Call listOfRequests() for all valid requests.", sep = ""))
      }
      params <- c(params, paramsI)
    }
    params <- unique(params)
    vals   <- NULL
    for (i in 1:length(params)){
      vals[i] <- valsAll[which(paramsAll == params[i])]
    }
  }
  
  for (j in 1:length(params)){
    # assignment of standard values for variables missing in input but required for calculation
    #if((!params[j] %in% namesLsCond) | (NA %in% get(params[j])) | (length(get(params[j]))==0)){
    if ((length(get(params[j])) > 1) & (NA %in% get(params[j]))){
      stop(paste("error: ", params[j], " is necessary for one or more of the indices required, but contains missing values!", sep=""))
    }
    if ((!params[j] %in% namesLsCond) | (NA %in% get(params[j])) | (length(get(params[j])) == 0)){
      assign(params[j], rep.int(vals[j], l))
      warning(paste("warning! ", params[j], " is necessary for one or more of the indices required, but was not given in input data. For the calculation it was set to the standard value of ", vals[j], " in all rows.", sep = ""))
    } else if (length(get(params[j])) == 1){
      assign(params[j], rep.int(get(params[j]), l))
    } else if (length(get(params[j])) != l){
      stop(paste("error: Length of ", params[j], "does not match!", sep = ""))
    }
  }
  
  #for(i in 1:l){vel[i] <- ifelse(vel[i] < 0, 0, vel[i])}
  
  if (length(request == 1) & request[1] == "all"){
    
    for (i in 1:l){
      # (5)
      giveDat <- data.frame(calcPMVPPD(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i]),
                            calc2Node(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], tu[i], obj = "set"),
                            calctnAuliciems(ta[i], tmmo[i]),
                            calctnHumphreysNV(tmmo[i]),
                            calctnHumphreysAC(tmmo[i]),
                            calctAdapt15251(trm[i]),
                            calcdTNZ(ht[i], wt[i], age[i], gender[i], clo[i], vel[i], tsk[i], ta[i], met[i], rh[i], deltaT =.1),
                            ATHBpmv = calcATHBpmv2015(trm[i], psych[i], ta[i], tr[i], vel[i], rh[i], met[i], wme[i]),
                            ATHBset = calcATHBset(trm[i], psych[i], ta[i], tr[i], vel[i], rh[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i]),
                            ATHBpts = calcATHBpts(trm[i], psych[i], ta[i], tr[i], vel[i], rh[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i]),
                            apmv = calcaPMV(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], apCoeff[i]),
                            epmv = calcePMV(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], epCoeff[i]),
                            ptsa = calcPtsa(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], tu[i], asCoeff[i]),
                            ptse = calcPtse(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], tu[i], esCoeff[i]),
                            pmvadj = calcPMVadj(ta[i], tr[i], vel[i], rh[i], clo[i], met[i]),
                            HBxst = calcHbExSteady(ta[i], tr[i], rh[i], vel[i], clo[i], met[i], tao[i], rho[i], frad[i], eps[i], ic[i], ht[i], wt[i], tcrI[i], tskI[i], basMet[i], warmUp[i], cdil[i], sigmatr[i])[33],
                            humidex = calcHumx(ta[i], rh[i])
                            
                            
      )
      if (i == 1){giveData<-giveDat}
      else{giveData<-rbind(giveData, giveDat)}
    }
    row.names(giveData) <- 1:i
  } else {
    for (i in 1:l){
      for (nparam in 1:length(request)){
        # (6)
        if (request[nparam] == "pmv" | request[nparam] == "ppd"){
          
          comfortData <- data.frame(calcPMVPPD(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i]))
          giveDat <- with(comfortData, get(request[nparam]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas <- cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "tnHumphreysNV"){
          
          giveDat <- calctnHumphreysNV(tmmo[i])
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas <- cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "tnHumphreysAC"){
          
          giveDat <- calctnHumphreysAC(tmmo[i])
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas <- cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "tAdapt15251"){
          
          giveDat <- calctAdapt15251(trm[i])
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "dTNZ" | request[nparam] == "dTNZTa" | request[nparam] == "dTNZTs"){
          
          comfortData <- data.frame(calcdTNZ(ht[i], wt[i], age[i], gender[i], clo[i], vel[i], tsk[i], ta[i], met[i], rh[i]))
          giveDat <- with(comfortData, get(request[nparam]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "ATHBpmv"){
          
          giveDat <- data.frame(calcATHBpmv2015(trm[i], psych[i], ta[i], tr[i], vel[i], rh[i], met[i], wme[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] =="ATHBset"){
          
          giveDat <- data.frame(calcATHBset(trm[i], psych[i], ta[i], tr[i], vel[i], rh[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "ATHBpts"){
          
          giveDat <- data.frame(calcATHBpts(trm[i], psych[i], ta[i], tr[i], vel[i], rh[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "apmv"){
          
          giveDat <- data.frame(calcaPMV(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], apCoeff[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "ptsa"){
          
          giveDat <- data.frame(calcPtsa(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], tu[i], asCoeff[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "epmv"){
          
          giveDat <- data.frame(calcePMV(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], epCoeff[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "ptse"){
          
          giveDat <- data.frame(calcPtse(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], tu[i], esCoeff[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "pmvadj"){
          
          giveDat <- data.frame(calcPMVadj(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "epCoeff"){
          metadj <- ifelse(asv[i]>0, met[i]*(1+asv[i]*(-.067)), met[i])
          comfortData <- data.frame(calcPMVPPD(ta[i], tr[i], vel[i], rh[i], clo[i], metadj, wme[i]))
          giveDat <- with(comfortData, get("pmv"))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "apCoeff"){
          comfortData <- data.frame(calcPMVPPD(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i]))
          giveDat <- with(comfortData, get("pmv"))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "esCoeff"){
          metadj <- ifelse(asv[i]>0, met[i]*(1+asv[i]*(-.067)), met[i])
          comfortData <- data.frame(calc2Node(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], obj = "set"))
          giveDat <- with(comfortData, get("pts"))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "asCoeff"){
          comfortData <- data.frame(calc2Node(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], obj = "set"))
          giveDat <- with(comfortData, get("pts"))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "pd"){
          
          comfortData <- data.frame(calc2Node(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], tu[i], obj = "set"))
          giveDat <- with(comfortData, get(request[nparam]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "HBxst"){
          
          comfortData <- data.frame(calcHbExSteady(ta[i], tr[i], rh[i], vel[i], clo[i], met[i], tao[i], rho[i], frad[i], eps[i], ic[i], ht[i], wt[i], tcrI[i], tskI[i], basMet[i], warmUp[i], cdil[i], sigmatr[i]))
          giveDat <- with(comfortData, get("xconss"))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else if (request[nparam] == "humidex"){
          
          giveDat <- calcHumx(ta[i], rh[i])
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
          
        } else {
          
          comfortData <- data.frame(calc2Node(ta[i], tr[i], vel[i], rh[i], clo[i], met[i], wme[i], pb[i], ltime[i], ht[i], wt[i], obj = "set"))
          giveDat <- with(comfortData, get(request[nparam]))
          if (nparam == 1){
            giveDatas<-giveDat
          } else {giveDatas<-cbind(giveDatas, giveDat)}
        }
      }
      if (i == 1){
        giveData<-giveDatas
      } else {giveData<-rbind(giveData, giveDatas)}
    } # end for nparam
    
    if (i > 1 | length(request) > 1){ #avoid warning that row.names are not unique in case of only one requested indices
      row.names(giveData) <- 1:i
    }
    giveData <- data.frame(giveData)
    #if(length(request)>1){
    colnames(giveData) <- request#}
    
  } # end else
  giveData
  #comfortData
} # end fct
comfind <- calcComfInd
