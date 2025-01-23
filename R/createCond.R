#' @title Creating a List with Standard Values
#' @description \code{createCond} creates a list with standard variables to be used as an input parameter for calculating comfort indices using the function \code{calcComfInd}.
#' @aliases createCond
#' @aliases createcond
#' @usage createCond(a = TRUE)
#' @usage createcond(a = TRUE)
#' @param a logical. If a = TRUE, function returns a list of standard conditions. If a = FALSE, function returns a list of empty variables which may be edited manually. See details for further information.
#' @details
#' \code{lsstrd} and \code{lsEmpty} contain the following elements
#' \tabular{llll}{
#'  Variable name \tab values in \code{lsstrd} \tab values in \code{lsEmpty} \tab description \cr
#'  ta \tab 25 \tab NA \tab Air temperature in (degree C) \cr
#'  tr  \tab 25 \tab NA \tab mean radiant temperature in (degree C) \cr
#'  vel \tab .1 \tab NA \tab Air velocity (m/s) \cr
#'  rh \tab 50 \tab NA \tab Relative Humidity (\%) \cr
#'  clo \tab .5 \tab NA \tab clothing (do) \cr
#'  met \tab 1 \tab NA \tab metabolic rate (met) \cr
#'  wme \tab 0 \tab NA \tab External work (met) \cr
#'  tu \tab 40 \tab NA \tab turbulence intensity (\%) \cr
#'  tmmo \tab 15 \tab NA \tab mean monthly outdoor temperature in (degree C) \cr
#'  ltime \tab 60 \tab NA \tab Exposure time (min) \cr
#'  pb \tab 760 \tab NA \tab Barometric pressure (torr) \cr
#'  wt \tab 70 \tab NA \tab weight (Kg) \cr
#'  ht \tab 171 \tab NA \tab height (cm) \cr
#'  trm \tab 15 \tab NA \tab Running mean outdoor temperature in (degree C) \cr
#'  age \tab 21 \tab NA \tab age (years) \cr
#'  gender \tab 1 \tab NA \tab gender (female = 1) \cr
#'  tsk \tab 35 \tab NA \tab mean skin temperature in (degree C) \cr
#'  psych \tab -1.4 \tab NA \tab factor related to fixed effect on perceived control \cr
#'  apCoeff \tab .293 \tab NA \tab adaptive coefficient for pmv \cr
#'  epCoeff \tab .9 \tab NA \tab expectancy factor for pmv \cr
#'  asCoeff \tab .2 \tab NA \tab adaptive coefficient for set \cr
#'  esCoeff \tab 1.3 \tab NA \tab expectancy factor for set \cr
#'  asv \tab 1.5 \tab NA \tab actual sensation vote (0 = neutral) \cr
#'  tao \tab 5 \tab NA \tab outdoor air temperature \cr
#'  rho \tab 70 \tab NA \tab outdoor relative humidity \cr
#'  frad \tab .7 \tab NA \tab 0.7(for seating), 0.73(for standing) [-]  \cr
#'  eps \tab .95 \tab NA \tab emissivity [-] \cr
#'  ic \tab 1.085 \tab NA \tab 1.084 (average permeability), 0.4 (low permeability)  \cr
#'  tcr \tab 37 \tab NA \tab initial values for core temp \cr
#'  tsk \tab 36 \tab NA \tab initial values for skin temperature \cr
#'  basMet \tab 58.2 \tab NA \tab basal metabolic rate \cr
#'  warmUp \tab 60 \tab NA \tab length of warm up period, i.e. number of times, loop is running for HBx calculation \cr
#'  cdil \tab 100 \tab NA \tab value for cdil in 2-node model of Gagge (applied in calculation of HbEx) \cr
#'  sigmatr \tab .25 \tab NA \tab value for cdil in 2-node model of Gagge (applied in calculation of HbEx)
#' }
#' @returns
#' \item{lsstrd}{List, which is created for a = TRUE; contains standard conditions.}
#' \item{lsEmpty}{List, which is created for a = FALSE; contains empty variables to be modified manually.} indices listed as \code{request}. For details see details above.
#' @examples
#' ## Creating list with standard variables
#' createCond()
#' ## Creating list with empty values
#' createCond(a = FALSE)
#' @author Sophia Mueller and Marcel Schweiker.
#' @seealso see also \code{\link{calcComfInd}}
#' @references For references see individual functions.
#' @export

createCond <- function(a = TRUE) {
  # (7)
  if (a == TRUE) {
    ta <- 25 # Air temperature (degree C)
    tr <- 25 # mean radiant temperature (degree C)
    vel <- .1 # Air velocity (m/s)
    rh <- 50 # Relative Humidity (%)
    clo <- .5 # clothing (do)
    met <- 1.1 # metabolic rate (met)
    wme <- 0 # External work (met)
    tu <- 40 # turbulence intensity (%)
    tmmo <- 15 # mean monthly outdoor temperature (degree C)
    ltime <- 60 # Exposure time (min)
    pb <- 760 # Barometric pressure (torr)
    wt <- 70 # weight (Kg)
    ht <- 171 # height (cm)
    trm <- 15 # Running mean outdoor temperature (degree C)
    age <- 21 # age (years)
    gender <- 1 # gender (female = 1)
    tsk <- 35 # mean skin temperature [degree C]
    psych <- (-1.4) # factor related to fixed effect on perceived control
    apCoeff <- .293 # adaptive coefficient for pmv
    epCoeff <- .9 # expectancy factor for pmv
    asCoeff <- (.2) # adaptive coefficient for set
    esCoeff <- 1.3 # expectancy factor for set
    asv <- 1.5 # actual sensation vote (0 = neutral)
    tao <- 5 # outdoor air temperature
    rho <- 70 # outdoor relative humidity
    frad <- .7 # 0.7(for seating), 0.73(for standing) [-]
    eps <- .95 # emissivity [-]
    ic <- 1.085 # 1.084 (average permeability), 0.4 (low permeability)
    tcrI <- 37 # initial values for core temp
    tskI <- 36 # initial values for skin temperature
    basMet <- 58.2 # basal metabolic rate
    warmUp <- 60 # length of warm up period, i.e. number of times, loop is running for HBx calculation
    cdil <- 100
    sigmatr <- .25

    print("A list with standard values was created. It contains standard room conditions.")
    print("For using the function 'calcComfInd', you may edit the values in that list and use this one as parameter value or create one on your own.")

    # (8)
    list(ta = ta, tr = tr, vel = vel, rh = rh, clo = clo, met = met, wme = wme, tu = tu, tmmo = tmmo, ltime = ltime, pb = pb, wt = wt, ht = ht, trm = trm, age = age, gender = gender, tsk = tsk, psych = psych, apCoeff = apCoeff, epCoeff = epCoeff, asCoeff = asCoeff, esCoeff = esCoeff, asv = asv, tao = tao, rho = rho, frad = frad, eps = eps, ic = ic, tcrI = tcrI, tskI = tskI, basMet = basMet, warmUp = warmUp, cdil = cdil, sigmatr = sigmatr)
  } else {
    # (9)
    ta <- NA # Air temperature (degree C)
    tr <- NA # mean radiant temperature (degree C)
    vel <- NA # Air velocity (m/s)
    rh <- NA # Relative Humidity (%)
    clo <- NA # clothing (do)
    met <- NA # metabolic rate (met)
    wme <- NA # External work (met)
    tu <- NA # turbulence intensity (%)
    tmmo <- NA # mean monthly outdoor temperature (degree C)
    ltime <- NA # Exposure time (min)
    pb <- NA # Barometric pressure (torr)
    wt <- NA # weight (Kg)
    ht <- NA # height (cm)
    trm <- NA # Running mean outdoor temperature (degree C)
    age <- NA # age (years)
    gender <- NA # gender (female = 1)
    tsk <- NA # mean skin temperature [degree C]
    psych <- NA # factor related to fixed effect on perceived control
    apCoeff <- NA # adaptive coefficient
    epCoeff <- NA # expectancy factor
    asCoeff <- NA # adaptive coefficient for set
    esCoeff <- NA # expectancy factor for set
    asv <- NA # actual sensation vote (0 = neutral)
    tao <- NA # outdoor air temperature
    rho <- NA # outdoor relative humidity
    frad <- NA # 0.7(for seating), 0.73(for standing) [-]
    eps <- NA # emissivity [-]
    ic <- NA # 1.084 (average permeability), 0.4 (low permeability)
    tcrI <- NA # initial values for core temp
    tskI <- NA # initial values for skin temperature
    basMet <- NA # basal metabolic rate
    warmUp <- NA # length of warm up period, i.e. number of times, loop is running for HBx calculation
    cdil <- NA
    sigmatr <- NA

    print("An empty list was created. You need to edit it for using it in the function 'calcComfInd'.")

    # (10)
    list(ta = ta, tr = tr, vel = vel, rh = rh, clo = clo, met = met, wme = wme, tu = tu, tmmo = tmmo, ltime = ltime, pb = pb, wt = wt, ht = ht, trm = trm, age = age, gender = gender, tsk = tsk, psych = psych, apCoeff = apCoeff, epCoeff = epCoeff, asCoeff = asCoeff, esCoeff = esCoeff, asv = asv, tao = tao, rho = rho, frad = frad, eps = eps, ic = ic, tcrI = tcrI, tskI = tskI, basMet = basMet, warmUp = warmUp, cdil = cdil, sigmatr = sigmatr)
  }
}

createcond <- createCond

listOfRequests <- function() {
  print(c("pmv", "ppd", "tnHumphreysNV", "tnHumphreysAC", "tAdapt15251", "dTNZ", "dTNZTa", "dTNZts", "ATHBpmv", "ATHBset", "ATHBpts", "apmv", "ptsa", "epmv", "ptse", "epCoeff", "apCoeff", "esCoeff", "asCoeff", "set", "et", "tsens", "disc", "pd", "ps", "pts", "HBxst", "pmvadj", "humidex"))
}
# listOfRequests()
