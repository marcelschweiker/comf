#' Human Body Exergy Consumption Rate Using Steady State Method
#'
#' @aliases calcHbExSteady Hbexsteady HbExSteady HbEx
#' @description \code{calcHbExSteady} calculates the human body exergy
#' consumption rate  in W/m2 using steady state method based on a set of
#' environmental variables.
#'
#' @usage
#' calcHbExSteady(ta, tr, rh, vel, clo, met, tao, rho, frad = 0.7, eps = 0.95,
#' ic = 1.085, ht = 171, wt = 70, tcr = 37, tsk = 36, basMet = 58.2, warmUp = 60,
#' cdil = 100, sigmatr = 0.25)
#'
#' @param ta a numeric value presenting air temperature in [degree C]
#' @param tr a numeric value presenting mean radiant temperature in [degree C]
#' @param rh a numeric value presenting relative humidity [\%]
#' @param vel a numeric value presenting air velocity in [m/s]
#' @param clo a numeric value presenting clothing insulation level in [clo]
#' @param met a numeric value presenting metabolic rate in [met]
#' @param tao a numeric value presenting outdoor air temperature in [degree C]
#' @param rho a numeric value presenting outdoor relative humidity [\%]
#' @param frad a numeric value presenting the fraction of body exposed to
#' radiation 0.7(for seating), 0.73(for standing) [-]
#' @param eps a numeric value presenting emissivity [-]
#' @param ic a numeric value presenting permeability of clothing: 1.084
#' (average permeability), 0.4 (low permeability)
#' @param ht a numeric value presenting body height in [cm]
#' @param wt a numeric value presenting body weight in [kg]
#' @param tcr a numeric value presenting initial value for core temperature in [degree C]
#' @param tsk a numeric value presenting initial value for skin temperature in [degree C]
#' @param basMet a numeric value presenting basal metabolic rate in [met]
#' @param warmUp a numeric value presenting length of warm up period, i.e. number
#' of times, loop is running for HBx calculation
#' @param cdil a numeric value presenting value for cdil in 2-node model of Gagge
#' @param sigmatr a numeric value presenting value for cdil in 2-node model of Gagge
#'
#' @return Returns a data.frame with the following columns \cr
#' \cr
#' Exergy input\cr
#' \code{xInmets} Exergy input through metabolism [W/m2]\cr
#' \code{xInmetwcs} Label warm/ cold for exergy input through metabolism [W/m2]\cr
#' \code{xInAIRwcs} Exergy input through inhaled humid air [W/m2]\cr
#' \code{xInAIRwcwcs} Label warm/ cold for exergy input through inhaled humid air [W/m2]\cr
#' \code{xInAIRwds} Exergy input through inhaled dry air [W/m2]\cr
#' \code{xInAIRwdwds} Label wet/ dry for exergy input through inhaled dry air [W/m2]\cr
#' \code{xInLUNGwcs} Exergy input through water lung [W/m2]\cr
#' \code{xInLUNGwcwcs} Label warm/ cold for exergy input through water lung [W/m2]\cr
#' \code{xInLUNGwds} Exergy input through water lung [W/m2]\cr
#' \code{xInLUNGwdwds} Label wet/ dry for exergy input through water lung [W/m2]\cr
#' \code{xInsheLLwcs} Exergy input through water from sweat [W/m2]\cr
#' \code{xInsheLLwcwcs} Label warm/ cold for exergy input through water from sweat [W/m2]\cr
#' \code{xInsheLLwds} Exergy input through water from sweat [W/m2]\cr
#' \code{xInsheLLwdwds} Label wet/ dry for exergy input through water from sweat [W/m2]\cr
#' \code{xInraDs} Exergy input through radiation [W/m2]\cr
#' \code{xInraDwcs} Label warm/ cold for exergy input through radiation [W/m2]\cr
#' \code{xIntotaLs} total exergy input [W/m2]\cr
#' \cr
#' Exergy output\cr
#' \code{xoutstorecores} Exergy stored in core [W/m2]\cr
#' \code{xoutstoreshels} Exergy stored in shell [W/m2]\cr
#' \code{xoutaIRwcs} Exergy output through exhaled humid air [W/m2]\cr
#' \code{xoutaIRwcwcs} Label warm/ cold for exergy output through exhaled humid air [W/m2]\cr
#' \code{xoutaIRwds} Exergy output through exhaled dry air [W/m2]\cr
#' \code{xoutaIRwdwds} Label wet/ dry for exergy output through exhaled dry air [W/m2]\cr
#' \code{xoutswEATwcs} Exergy output through water vapour from sweat [W/m2]\cr
#' \code{xoutswEATwcwcs} Label warm/ cold for exergy output through water vapour from sweat [W/m2]\cr
#' \code{xoutswEATwds} Exergy output through water vapour from sweat [W/m2]\cr
#' \code{xoutswEATwdwds} Label wet/ dry for exergy output through water vapour from sweat [W/m2]\cr
#' \code{xoutraDs} Exergy output through radiation [W/m2]\cr
#' \code{xoutraDwcs} Label warm/ cold for exergy output through radiation [W/m2]\cr
#' \code{xoutCONVs} Exergy output through convection [W/m2]\cr
#' \code{xoutCONVwcs} Label warm/ cold for exergy output through convection [W/m2]\cr
#' \code{xouttotaLs} total exergy output [W/m2]\cr
#' \cr
#' Exergy balance\cr
#' \code{xconss} total exergy consumption [W/m2]\cr
#' \code{xConsumption} total exergy consumption [W/m2]\cr
#' \cr
#' Additional values\cr
#' \code{tsks} Calculated skin temperature [degree C]\cr
#' \code{tcrs} Calculated core temperature [degree C]\cr
#' \code{ws} Calculated skin wettedness [degree C]\cr
#'
#' @export
#'
#' @note
#' According to Gagge's paper (1973), the value of 'cdil' may vary between 75
#' and 225 and 'sigma-tr' between 0.25 and 0.75. There is a note in the appendix
#' of his paper saying two things: 1) whatever the values taken for cdil and
#' sigma-tr, there must be no significant change in resulting thermal equilibrium.
#' But, the values taken for cdil and sigmaTr do affect time to equilibrium.
#' According to the analysis of Schweiker et al. (2016), the values of 100 and
#' .25 lead to the best fit of calculated and observed skin temperature.
#'
#' @author
#' This function is based on a VBA code developed by Masanori Shukuya.
#' transformation of VBA-code and Excel procedures into R syntax by Marcel
#' Schweiker.
#'
#' @references
#' Schweiker, Kolarik, Dovjak & Shukuya (2016) <doi:10.1016/j.enbuild.2016.01.002>
#'
#' Shukuya (2015) Calculation of human body-core and skin-layer temperatures under
#' unsteady-state conditions-for unsteady-state human-body exergy analysis-,
#' internal report of exergy-research group, Tech. rep.
#'
#' @seealso
#' see also \code{\link{calcComfInd}}, \code{\link{calcHbExUnsteady}}
#'
#' @examples
#' ## Calculation of human body exergy consumption rate
#' calcHbExSteady(22, 24, 50, .1, .8, 1.2, 5, 80)
#'
#' ## Calculation of multiple values
#' dfData <- data.frame(ta = c(20:25), tr = c(20:25))
#' dfResult <- calcHbExSteady(22, 24, 50, .1, .8, 1.2, 5, 80)
#' for (i in 1:nrow(dfData)) {
#'   dfResult[i, ] <- calcHbExSteady(dfData$ta[i], dfData$tr[i], 50, .1, .5, 1.1, 5, 80)
#' }
####################################
# main program
#############################################
#' #
# This is a program for the calculation of human-body core and skin temperatures
# and also clothing surface temperature based on the two-node model
# originally developed by Gagge et al.
# The program has been developed so that it fits the calculation of human-body
# exergy balance under unsteady-state conditions.
# 1st ver.  ms 13th February, 2013
#' #
# This program has been further extended to be able to include the human-body exergy balance.
#  ms 11th may, 2014
#' #
# This version is for un-steady state exergy calculation.
#  ms 30th June, 2014//18th February, 2015
#' #
# transfer of VBA-code to R-code by m. schweiker march, 2015
#' #
###############################################
calcHbExSteady <- function(ta, tr, rh, vel, clo, met, tao, rho, frad = .7,
                           eps = .95, ic = 1.085, ht = 171, wt = 70, tcr = 37, tsk = 36,
                           basMet = 58.2, warmUp = 60, cdil = 100, sigmatr = .25) {
  # definition of output variables
  # Exergy input
  xInmets <- xInmetwcs <- xInAIRwcs <- xInAIRwcwcs <- xInAIRwds <- xInAIRwdwds <- xInLUNGwcs <- xInLUNGwcwcs <- xInLUNGwds <- xInLUNGwdwds <- xInsheLLwcs <- xInsheLLwcwcs <- xInsheLLwds <- xInsheLLwdwds <- xInraDs <- xInraDwcs <- xIntotaLs <- NA

  # Exergy output
  xoutstorecores <- xoutstoreshels <- xoutaIRwcs <- xoutaIRwcwcs <- xoutaIRwds <- xoutaIRwdwds <- xoutswEATwcs <- xoutswEATwcwcs <- xoutswEATwds <- xoutswEATwdwds <- xoutraDs <- xoutraDwcs <- xoutCONVs <- xoutCONVwcs <- xouttotaLs <- NA

  # balance and additional variables
  xconss <- tsks <- tcrs <- ws <- NA

  tko <- 273.15
  tskSet <- 33.7
  tcrSet <- 36.8
  lr <- 16.5 * 10^(-3)
  # cdil <- 200 # may vary between 75 and 225
  # sigmatr <- 0.5; # may vary between .25 and .75
  csw <- 170
  mAir <- 28.97 * 0.001
  mWater <- 18.015 * 0.001
  rGas <- 8.31446 # [J/(molK)]
  row <- 1000
  Roa <- 1.2 # [kg/m3]
  cpBody <- 3490
  cpa <- 1005
  cpv <- 1846
  cpw <- 4186 # [J/(kgK)]
  PO <- 101325 # [N/m2<-J/m3]
  aBody <- 0.008883 * ht^0.663 * wt^0.444 # radiation area of human body - taking account for covered parts by other parts of body (e.g. inner parts of arm) - coefficients taken from Fanger

  rMA <- wt / aBody

  i <- 1

  icl <- clo # clothing insulation [clo]
  va <- vel # Indoor air velocity [m/s]
  met <- met # metabolic rate [met]
  tmr <- tr # mean radiant temp [degree C]
  ta <- ta # air temp [degree C]
  phia <- rh # relative humidity indoors [%]
  toEnv <- tao # outdoor temp [degree C]
  phiaEnv <- rho # relative humidity outdoors [%]

  # warm-up period (here 30 minutes) - see also discussion in paper
  for (j in 1:warmUp) {
    # extract single value from time series data
    dtCal <- 60
    qmet <- metaTherm(met, basMet)
    fcl <- 1 + 0.15 * icl
    rcl <- 0.155 * icl

    TKa <- ta + tko
    tkmr <- tmr + tko

    tkoEnv <- toEnv + tko
    pvEnv <- pVapor(toEnv, phiaEnv) #--------module_1 function see Book Shukuya p. 268

    hr <- 6.13 * frad * eps # radiative heat transfer coefficient
    hc <- hcvG(va, met, basMet) # convective heat transfer coefficient #--------module_1
    top <- (hr * tmr + hc * ta) / (hr + hc)
    ffcl <- 1 / (1 + 0.155 * icl * fcl * (hr + hc))
    fpcl <- 1 / (1 + 0.155 * icl * fcl * hc / ic) # related to evaporation
    cl <- 1 / (1 + 0.155 * icl * fcl * hc / ic)
    im <- hc * fpcl / ((hr + hc) * ffcl)

    iclStar <- 0.6
    fclStar <- 1 + 0.15 * iclStar
    hcStar <- hcvG(0.1, 1, basMet)
    ffclStar <- 1 / (1 + 0.155 * iclStar * fclStar * (hr + hcStar))
    fpclStar <- 1 / (1 + 0.155 * iclStar * fclStar * hcStar / ic)
    imStar <- hcStar * fpclStar / ((hr + hcStar) * ffclStar)
    cres <- 0.0014 * (basMet * met) * (34 - ta) # heat transfer to environment by convection in relation to respiration (empirical equation by Fanger)

    pva <- pVapor(ta, phia) #--------module_1
    eres <- 0.0173 * (basMet * met) * (5.87 - pva / 1000) # heat removed by evaporation in relation to respiration (empirical equation by Fanger)


    # Else
    qShiv <- mshiv(tcrSet, tskSet, tcr, tsk) #--------module_3
    vblS <- vblCdilStr(cdil, sigmatr, tcrSet, tskSet, tcr, tsk) # blood flow rate #--------module_2
    # next line is core of Gagge model. If blood flow becomes lower, than skin layer gets more dominant
    alfaSk <- 0.0418 + 0.745 / (vblS + 0.585) # factor to adjust for difference in dominance
    kS <- 5.28 + 1.163 * vblS # conductance between core and skin layer
    Qcr <- (1 - alfaSk) * rMA * cpBody
    Qsk <- alfaSk * rMA * cpBody # heat capacity of core and skin layer

    tcrN <- (1 - dtCal * kS / Qcr) * tcr + dtCal / Qcr * (qmet + qShiv - (cres + eres) + kS * tsk) # core temperature at time step before step calculated
    psks <- pVapor(tsk, 100) # saturated water vapour pressure at skin surface calculated with pure water , i.e. adaptive processes such as less salt in sweat might be put heresee also p. 281 of book Shukuya #--------module_1
    emax <- fpcl * (fcl * lr * hc) * (psks - pva) # max rate of water dispersion when the whole skin surface is wet
    tb <- alfaSk * tsk + (1 - alfaSk) * tcr # average body temperature using calculated tsk and tcr
    tbSet <- alfaSk * tsk + (1 - alfaSk) * tcrSet # average body temperature using set-point values for tsk and tcr
    ersw <- csw * (tb - tbSet) * exp((tsk - tskSet) / 10.7) # amount of sweat secretion

    w <- 0.06 + 0.94 * ersw / emax
    if (w < 0.06) {
      w <- 0.06
    }
    if (1 < w) {
      w <- 1
    }

    DTQ <- dtCal / Qsk
    tskN <- (1 - DTQ * kS - DTQ * fcl * ffcl * (hr + hc)) * tsk - DTQ * w * fcl * lr * hc * fpcl * psks + DTQ * (kS * tcr + fcl * (hr + hc) * ffcl * top + w * fcl * lr * hc * fpcl * pva) # tsk in next step
    tclN <- ((1 / rcl) * tskN + fcl * hr * tmr + fcl * hc * ta) / (1 / rcl + fcl * hr + fcl * hc)
    #
    # Exergy balance
    #
    # Thermal exergy generation by metabolism
    #
    tkcrN <- tcrN + tko
    tkskN <- tskN + tko
    tkclN <- tclN + tko
    metaEnergy <- qmet + qShiv
    xMet <- metaEnergy * (1 - tkoEnv / tkcrN)
    xMetwc <- wcXCheck(tkcrN, tkoEnv)

    # Inhaled humid air

    Vin <- 1.2 * 10^(-6) * metaEnergy # Volume of air intake [V/s]
    cpav <- cpa * (mAir / (rGas * TKa)) * (PO - pva) + cpv * (mWater / (rGas * TKa)) * pva
    xInhaleWc <- Vin * wcEx(cpav, TKa, tkoEnv)
    xInhaleWcwc <- wcXCheck(TKa, tkoEnv) #--------module_4

    xInhaleWd <- Vin * wdEx(TKa, tkoEnv, pva, pvEnv)
    xInhaleWdwd <- wdXCheck(pva, pvEnv) #--------module_4

    # Liquid water generated in the core by metabolism to be dispersed into the exhaled air

    VwCore <- Vin * (0.029 - 0.049 * 10^(-4) * pva)
    xLwCoreWc <- VwCore * Roa * wcEx(cpw, tkcrN, tkoEnv)
    xLwCoreWcwc <- wcXCheck(tkcrN, tkoEnv) #--------module_4

    pvs_env <- pVapor(toEnv, 100)
    xLwCoreWet <- VwCore * Roa * rGas / mWater * tkoEnv * log(pvs_env / pvEnv)
    xLwCoreWetwd <- "wet"

    # Liquid water generated in the shell by metabolism to be secreted as sweat

    vwShellrow <- w * emax / (2450 * 1000)
    xLwShellWc <- vwShellrow * wcEx(cpw, tkskN, tkoEnv)
    xLwShellWcwc <- wcXCheck(tkskN, tkoEnv) #--------module_4

    xLwShellWd <- vwShellrow * wdExLw(tkoEnv, pvs_env, pva, pvEnv)
    xLwShellWdwd <- wdXCheck(pva, pvEnv) #--------module_4

    # radiant exergy input

    xInRad <- fcl * hr * (tkmr - tkoEnv)^2 / (tkmr + tkoEnv)
    xInRadwc <- wcXCheck(tkmr, tkoEnv)

    # total exergy input

    xIntotal <- xMet + xInhaleWc + xInhaleWd + xLwCoreWc + xLwCoreWet + xLwShellWc + xLwShellWd + xInRad

    # Exergy stored

    xStCore <- Qcr * (1 - tkoEnv / tkcrN) * (tcrN - tcr) / dtCal
    xStShell <- Qsk * (1 - tkoEnv / tkskN) * (tskN - tsk) / dtCal

    # Exhaled humid air

    pvssCr <- pVapor(tcrN, 100)
    cpav <- cpa * (mAir / (rGas * tkcrN)) * (PO - pvssCr) + cpv * (mWater / (rGas * tkcrN)) * pvssCr
    xExhaleWc <- Vin * wcEx(cpav, tkcrN, tkoEnv)
    xExhaleWcwc <- wcXCheck(tkcrN, tkoEnv) #--------module_4

    xExhaleWd <- Vin * wdEx(tkcrN, tkoEnv, pvssCr, pvEnv)
    xExhaleWdwd <- wdXCheck(pvssCr, pvEnv) #--------module_4

    # water vapor originating from the sweat and humid air containing the evaporated sweat

    xSweatWc <- vwShellrow * wcEx(cpv, tkclN, tkoEnv)
    xSweatWcwc <- wcXCheck(tkclN, tkoEnv) #--------module_4

    xSweatWd <- vwShellrow * wdExLw(tkoEnv, pva, pva, pvEnv)
    xSweatWdwd <- wdXCheck(pva, pvEnv) #--------module_4

    # radiant exergy output

    xOutRad <- fcl * hr * (tkclN - tkoEnv)^2 / (tkclN + tkoEnv)
    xOutRadwc <- wcXCheck(tkclN, tkoEnv)

    # Exergy transfer by convection

    xOutConv <- fcl * hc * (tkclN - TKa) * (1. - tkoEnv / tkclN)
    xOutConvwc <- wcXCheck(tkclN, tkoEnv)

    xouttotal <- xStCore + xStShell + xExhaleWc + xExhaleWd + xSweatWc + xSweatWd + xOutRad + xOutConv

    xConsumption <- xIntotal - xouttotal

    # etStar <- calcet(top, ta, phia, w, im, 50, im); #--------module_4

    tcr <- tcrN
    tsk <- tskN
  }

  # Output values
  # Exergy input
  xInmets[i] <- signif(xMet, 4) # metabolism
  xInmetwcs[i] <- xMetwc # metabolism warm/cold
  xInAIRwcs[i] <- signif(xInhaleWc, 4) # inhaled humid air
  xInAIRwcwcs[i] <- xInhaleWcwc
  xInAIRwds[i] <- signif(xInhaleWd, 4) #
  xInAIRwdwds[i] <- xInhaleWdwd # wet/dry
  xInLUNGwcs[i] <- signif(xLwCoreWc, 4) # water lung
  xInLUNGwcwcs[i] <- xLwCoreWcwc
  xInLUNGwds[i] <- signif(xLwCoreWet, 4)
  xInLUNGwdwds[i] <- xLwCoreWetwd
  xInsheLLwcs[i] <- signif(xLwShellWc, 4) # water sweat
  xInsheLLwcwcs[i] <- xLwShellWcwc
  xInsheLLwds[i] <- signif(xLwShellWd, 4)
  xInsheLLwdwds[i] <- xLwShellWdwd
  xInraDs[i] <- signif(xInRad, 4) # radiation in
  xInraDwcs[i] <- xInRadwc
  xIntotaLs[i] <- signif(xIntotal, 4) # totaL exergy input

  # Exergy output
  xoutstorecores[i] <- signif(xStCore, 4) # exergy stored
  xoutstoreshels[i] <- signif(xStShell, 4)
  xoutaIRwcs[i] <- signif(xExhaleWc, 4) # exhaled humid air
  xoutaIRwcwcs[i] <- xExhaleWcwc
  xoutaIRwds[i] <- signif(xExhaleWd, 4)
  xoutaIRwdwds[i] <- xExhaleWdwd
  xoutswEATwcs[i] <- signif(xSweatWc, 4) # water vapour
  xoutswEATwcwcs[i] <- xSweatWcwc
  xoutswEATwds[i] <- signif(xSweatWd, 4)
  xoutswEATwdwds[i] <- xSweatWdwd
  xoutraDs[i] <- signif(xOutRad, 4) # radiation out
  xoutraDwcs[i] <- xOutRadwc
  xoutCONVs[i] <- signif(xOutConv, 4) # convection
  xoutCONVwcs[i] <- xOutConvwc
  xouttotaLs[i] <- signif(xouttotal, 4) # totaL exergy out

  # balance
  xconss[i] <- signif(xConsumption, 4) # exergy consumPtion total
  tsks[i] <- signif(tsk, 4)
  tcrs[i] <- signif(tcr, 4)
  ws[i] <- signif(w, 4)

  # setStar <- calcet(top, ta, phia, w, im, 50, imStar);
  resultsst <- data.frame(
    # Output values

    # setStar,
    # etStar,

    # Exergy input
    xInmets, # metabolism
    xInmetwcs, # metabolism warm/cold
    xInAIRwcs, # inhaled humid air
    xInAIRwcwcs,
    xInAIRwds, #
    xInAIRwdwds, # wet/dry
    xInLUNGwcs, # water lung
    xInLUNGwcwcs,
    xInLUNGwds,
    xInLUNGwdwds,
    xInsheLLwcs, # water sweat
    xInsheLLwcwcs,
    xInsheLLwds,
    xInsheLLwdwds,
    xInraDs, # radiation in
    xInraDwcs,
    xIntotaLs, # totaL exergy input

    # Exergy output
    xoutstorecores, # exergy stored core
    xoutstoreshels, # exergy stored shell
    xoutaIRwcs, # exhaled humid air
    xoutaIRwcwcs,
    xoutaIRwds,
    xoutaIRwdwds,
    xoutswEATwcs, # water vapour
    xoutswEATwcwcs,
    xoutswEATwds,
    xoutswEATwdwds,
    xoutraDs, # radiation out
    xoutraDwcs,
    xoutCONVs, # convection
    xoutCONVwcs,
    xouttotaLs, # totaL exergy out

    # balance
    xconss, # exergy consumPtion total
    xConsumption,
    tsks, # skin temperature
    tcrs, # core temperature
    ws, # skin wettedness
    stringsAsFactors = FALSE
  )

  resultsst
} # end of main program
###########################
