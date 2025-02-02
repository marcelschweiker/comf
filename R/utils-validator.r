#' Internal function
#' @aliases validateUTCI
#' @param ta the air temperature
#' @param tr the radiant temperature
#' @param vel the air velocity
#' @param rh the relative humidity
#' @noRd
validateUTCI <- function(ta, tr, vel, rh) {
  parameters <- c(ta, tr, vel, rh)

  # check if parameter is numeric and not null
  for (parameter in parameters) {
    if (!is.numeric(parameter) || is.null(parameter)) {
      warning(paste(parameter, " is not valid (has to be numeric and not null)"))
    }
  }

  # check range for ta & tr
  checkRange(ta, -50, 50)

  # check range for difference of ta & tr
  checkRange(tr - ta, -30.0, 70.0)

  # check range for vel
  checkRange(vel, 0.5, 17)

  # check range for rh
  checkRange(rh, 0, 100)
}

#' @noRd
# validation for Solar Gain Input
validateSolarGainRange <- function(solAlt, solAzi, solRadDir, solTrans, fSvv,
                                   fBes, asw = 0.7, posture) {
  # check if parameters are  not null
  for (parameter in c(
    solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw,
    posture
  )) {
    if (is.null(parameter)) {
      warning(paste(parameter, " is not valid (cannot be null)"))
    }
  }

  # check if parameters are numeric
  for (parameter in c(solAlt, solAzi, solRadDir, solTrans, fSvv, fBes, asw)) {
    if (!is.numeric(parameter)) {
      warning(paste(parameter, " is not valid (has to be numeric)"))
    }
  }

  # validate posture input
  if (!posture %in% c("standing", "supine", "seated")) {
    stop("Posture has to be either standing, supine or seated")
  }

  # check range for Solar altitude
  checkRange(solAlt, 0, 90)

  # check range for Solar azimuth
  checkRange(solAzi, 0, 180)

  # check range for Direct-beam solar radiation
  checkRange(solRadDir, 200, 1000)

  # check ranges for solTrans, fSvv, fBes, asw
  for (parameter in c(solTrans, fSvv, fBes, asw)) {
    checkRange(parameter, 0, 1)
  }
}

# check range for parameters
#' @noRd
checkRange <- function(parameter, lower_bound, upper_bound) {
  if (parameter < lower_bound || parameter > upper_bound) {
    stop(paste(parameter, " is out of range. Has to be between ", lower_bound, " and ", upper_bound))
  }
}

#' @noRd
# validate calcPMV and calcPPD
validatePMVPPD <- function(pmv_ppd_data) {
  for (pmv_ppd_dataRow in pmv_ppd_data) {
    for (pmv_ppd_dataRow_index in 1:nrow(pmv_ppd_dataRow)) {
      dataRow <- pmv_ppd_dataRow[pmv_ppd_dataRow_index, ]
      inputs <- dataRow$inputs
      outputs <- dataRow$outputs
      pmv_output <- round(calcPMV(inputs$ta, inputs$tr, inputs$rh, inputs$v, inputs$met, inputs$clo), 1)
      ppd_output <- round(calcPPD(inputs$ta, inputs$tr, inputs$rh, inputs$v, inputs$met, inputs$clo), 1)
      if ((pmv_output != outputs$pmv) || (ppd_output != outputs$ppd)) {
        print(paste(
          "Outputs did not match for these inputs ->  ta:", inputs$ta,
          ",tr:", inputs$tr, ",rh:", inputs$rh, ",v:", inputs$v,
          ",met:", inputs$met, ",clo:", inputs$clo
        ))
        print(paste("Output from our function -> pmv:", pmv_output, ",ppd:", ppd_output))
        print(paste("Original outputs are -> pmv:", outputs$pmv, ",ppd: ", outputs$ppd))
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# validate Solar Gain
#' @noRd
validateSolarGain <- function(solar_gain_data) {
  for (solar_gain_dataRow in solar_gain_data) {
    for (solar_gain_dataRow_index in 1:nrow(solar_gain_dataRow)) {
      dataRow <- solar_gain_dataRow[solar_gain_dataRow_index, ]
      inputs <- dataRow$inputs
      outputs <- dataRow$outputs
      solar_gain_result <- calcSolarGain(inputs$alt, inputs$sharp, inputs$I_dir, inputs$t_sol, inputs$f_svv, inputs$f_bes, inputs$asa, inputs$posture)
      if ((solar_gain_result[1] != outputs$erf) || (solar_gain_result[2] != outputs$t_rsw)) {
        print(paste(
          "Outputs did not match for these inputs ->  alt:", inputs$alt,
          ",sharp:", inputs$sharp, ",I_dir:", inputs$I_dir, ",t_sol:",
          inputs$t_sol, ",f_svv:", inputs$f_svv, ",f_bes:",
          inputs$f_bes, ",asa:", inputs$asa, ",posture:", inputs$posture
        ))
        print(paste("Output from our function -> erf:", solar_gain_result[1], ",t_rsw:", solar_gain_result[2]))
        print(paste("Original outputs are -> erf:", outputs$erf, ",t_rsw: ", outputs$t_rsw))

        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# validate SET
#' @noRd
validateSET <- function(set_data) {
  for (setDataRow in set_data) {
    for (setDataRow_index in 1:nrow(setDataRow)) {
      dataRow <- setDataRow[setDataRow_index, ]
      inputs <- dataRow$inputs
      output <- dataRow$outputs$set
      set_result <- round(calcSET(inputs$ta, inputs$tr, inputs$v, inputs$rh, inputs$clo, inputs$met), 1)
      if (set_result != output) {
        print(paste(
          "Outputs did not match for these inputs ->  ta:", inputs$ta,
          ",tr:", inputs$tr, ",rh:", inputs$rh, ",v:", inputs$v,
          ",met:", inputs$met, ",clo:", inputs$clo
        ))
        print(paste("Output from our function -> set:", set_result))
        print(paste("Original output is -> set:", output))
        return(FALSE)
      }
    }
  }
}

# validate all methods
#' @noRd
validateAllFunctions <- function() {
  validationData <- as.data.frame(jsonlite::fromJSON("https://raw.githubusercontent.com/FedericoTartarini/validation-data-comfort-models/main/validationData.json"))
  pmv_ppd_validation_result <- validatePMVPPD(validationData$reference_data.pmv_ppd.data)
  solar_gain_validation_result <- validateSolarGain(validationData$reference_data.solar_gain.data)
  set_validation_result <- validateSET(validationData$reference_data.set.data)
}

# validate_all_functions()
