library(nleqslv)
#' Calculate the Steady Physiological Equivalent Temperature (PET)
#'
#' The PET is calculated using the Munich Energy-balance Model for Individuals,
#' PET is defined as the air temperature at which, in a typical indoor setting,
#' the heat budget of the human body is balanced with the same core and skin
#' temperature as under the complex outdoor conditions to be assessed.
#'
#' The PET was originally proposed by Hoppe. In 2018, Walther and Goestchel proposed a
#' correction of the original model, purging the errors in the PET calculation routine,
#' and implementing a state-of-the-art vapour diffusion model.
#'
#' @param tdb float, dry bulb air temperature, [degree]
#' @param tr float, mean radiant temperature, [degree]
#' @param v float, air speed, [m/s]
#' @param rh float, relative humidity, [%]
#' @param met float, metabolic rate, [met]
#' @param clo float, clothing insulation, [clo]
#' @param p_atm float, atmospheric pressure, default value 1013.25 [hPa]
#' @param position integer, position of the individual (1=sitting, 2=standing, 3=standing forced convection)
#' @param age integer, age in years, default 23
#' @param sex integer, gender (1 for male, 2 for female), default 1
#' @param weight float, body mass, [kg], default 75
#' @param height float, height, [m], default 1.8
#' @param wme float, external work, [W/(m2)], default 0
#'
#' @return PET, Steady-state PET under the given ambient conditions
#'
#' @examples
#' pet_steady(tdb = 20, tr = 20, rh = 50, v = 0.15, met = 1.37, clo = 0.5)
#' @references
#' Hoppe, P. (1999). The Physiological Equivalent Temperature - A Universal Index for the Biometeorological Assessment of the Thermal Environment.
#' Walther, G. and Goestchel, P. (2018). Improvement and Extension of the Munich Energy-balance Model for Individuals.
#' @author Code implemented into R by Chongyu Gan.
#' @export
calcPetSteady <- function(
    tdb,
    tr,
    v,
    rh,
    met,
    clo,
    p_atm = 1013.25,
    position = 1,
    age = 23,
    sex = 1,
    weight = 75,
    height = 1.8,
    wme = 0) {
  met_factor <- 58.2 # met conversion factor
  met <- met * met_factor # metabolic rate

  p_sat <- function(tdb) {
    # Constants for the equation
    c_to_k <- 273.15
    ta_k <- tdb + c_to_k

    c1 <- -5674.5359
    c2 <- 6.3925247
    c3 <- -0.9677843 * 1e-2
    c4 <- 0.62215701 * 1e-6
    c5 <- 0.20747825 * 1e-8
    c6 <- -0.9484024 * 1e-12
    c7 <- 4.1635019
    c8 <- -5800.2206
    c9 <- 1.3914993
    c10 <- -0.048640239
    c11 <- 0.41764768 * 1e-4
    c12 <- -0.14452093 * 1e-7
    c13 <- 6.5459673

    # Calculate pascals depending on temperature
    pascals <- ifelse(
      ta_k < c_to_k,
      exp(
        c1 / ta_k +
          c2 +
          ta_k * (c3 + ta_k * (c4 + ta_k * (c5 + c6 * ta_k))) +
          c7 * log(ta_k)
      ),
      exp(
        c8 / ta_k +
          c9 +
          ta_k * (c10 + ta_k * (c11 + ta_k * c12)) +
          c13 * log(ta_k)
      )
    )

    # Round the results
    return(round(pascals, 1))
  }

  bodySurfaceArea <- function(weight, height, formula = "dubois") {
    # Calculate the body surface area based on the selected formula
    if (formula == "dubois") {
      return(0.202 * (weight^0.425) * (height^0.725))
    } else if (formula == "takahira") {
      return(0.2042 * (weight^0.425) * (height^0.725))
    } else if (formula == "fujimoto") {
      return(0.1882 * (weight^0.444) * (height^0.663))
    } else if (formula == "kurazumi") {
      return(0.2440 * (weight^0.383) * (height^0.693))
    } else {
      stop(paste("The formula", formula
                 , "to calculate the body surface area does not exist."))
    }
  }

  vasomotricity <- function(t_cr, t_sk) {
    # Set values for core and skin temperature
    tc_set <- 36.6 # Core temperature set point
    tsk_set <- 34 # Skin temperature set point

    # Calculate the deviation from set points
    sig_skin <- tsk_set - t_sk
    sig_core <- t_cr - tc_set

    # If core temperature is below set point, do not reduce blood flow further
    if (sig_core < 0) {
      sig_core <- 0
    }

    # If skin temperature is above set point, do not increase blood flow further
    if (sig_skin < 0) {
      sig_skin <- 0
    }

    # Calculate the blood flow rate based on deviations
    m_blood <- (6.3 + 75.0 * sig_core) / (1.0 + 0.5 * sig_skin)

    # Limit the blood flow rate to a maximum of 90 L/m^2/h
    if (m_blood > 90) {
      m_blood <- 90
    }

    # Calculate alpha, a factor used to adjust the proportion of body mass
    alpha <- 0.0417737 + 0.7451833 / (m_blood + 0.585417)

    # Return the values as a list
    return(list(m_blood = m_blood, alpha = alpha))
  }

  sweat_rate <- function(t_body) {
    # Set temperature values
    tc_set <- 36.6 # Core temperature set point
    tsk_set <- 34 # Skin temperature set point
    # Calculation of the weighted body temperature
    tbody_set <- 0.1 * tsk_set + 0.9 * tc_set 

    # Calculate the deviation from the set body temperature
    sig_body <- t_body - tbody_set
    if (sig_body < 0) {
      # If body temperature is below the set point, sweating rate is zero
      sig_body <- 0
    }

    # Calculate the sweating flow rate using Gagge's model
    m_rsw <- 304.94 * sig_body
    # Cap the maximum sweating rate to 500 g/m2/h
    if (m_rsw > 500) {
      m_rsw <- 500
    }

    return(m_rsw)
  }

  solvePet <- function(tArr, tdb, tr, v = 0.1, rh = 50, met = 80, clo = 0.9, actual_environment = FALSE) {
    e_skin <- 0.99
    e_clo <- 0.95
    h_vap <- 2.42 * 10^6
    sbc <- 5.67 * 10^-8
    cb <- 3640

    t_cr <- tArr[1]
    t_sk <- tArr[2]
    t_clo <- tArr[3]

    e_bal_vec <- numeric(3)

    a_dubois <- bodySurfaceArea(weight, height)

    metFemale <- 3.19 * weight^0.75 * (1.0 + 0.004 * (30 - age) + 0.018 * ((height * 100) / weight^(1 / 3) - 42.1))
    metMale <- 3.45 * weight^0.75 * (1.0 + 0.004 * (30 - age) + 0.01 * ((height * 100) / weight^(1 / 3) - 43.4))

    metCorrection <- ifelse(sex == 1, metMale, metFemale)

    he <- (met + metCorrection) / a_dubois
    h <- he * (1.0 - wme)

    i_m <- 0.38
    fcl <- 1 + 0.31 * clo
    f_a_cl <- (173.51 * clo - 2.36 - 100.76 * clo^2 + 19.28 * clo^3) / 100
    a_clo <- a_dubois * f_a_cl + a_dubois * (fcl - 1)

    f_eff <- ifelse(position == 2, 0.696, 0.725)
    a_r_eff <- a_dubois * f_eff

    vpa <- rh / 100 * p_sat(tdb) / 100
    if (!actual_environment) {
      vpa <- 12 # Standard environment
    }

    hc <- 2.67 + 6.5 * v^0.67
    if (position == 2) {
      hc <- 2.26 + 7.42 * v^0.67
    } else if (position == 3) {
      hc <- 8.6 * v^0.513
    }

    h_cc <- 3.0 * (p_atm / 1013.25)^0.53
    hc <- max(h_cc, hc)
    hc <- hc * (p_atm / 1013.25)^0.55

    tExp <- 0.47 * tdb + 21
    dVentPulm <- he * 1.44 * 10^-6
    cRes <- 1010 * (tdb - tExp) * dVentPulm
    vpExp <- p_sat(tExp) / 100
    qRes <- 0.623 * h_vap / p_atm * (vpa - vpExp) * dVentPulm
    ere <- cRes + qRes

    alpha <- vasomotricity(t_cr, t_sk)$alpha
    tbody <- alpha * t_sk + (1 - alpha) * t_cr

    r_cl <- clo / 6.45
    y <- ifelse(f_a_cl > 1, 1, ifelse(clo >= 2, 1, ifelse(clo > 0.6, (height - 0.2) / height, ifelse(clo > 0.3, 0.5, 0.1))))
    r2 <- a_dubois * (fcl - 1 + f_a_cl) / (6.28 * height * y)
    r1 <- f_a_cl * a_dubois / (6.28 * height * y)
    di <- r2 - r1
    htcl <- 6.28 * height * y * di / (r_cl * log(r2 / r1) * a_clo)

    qmsw <- sweat_rate(tbody)
    esw <- h_vap / 1000 * qmsw / 3600
    p_v_sk <- p_sat(t_sk) / 100
    lr <- 16.7 * 10^-1
    he_diff <- hc * lr
    fecl <- 1 / (1 + 0.92 * hc * r_cl)
    e_max <- he_diff * fecl * (p_v_sk - vpa)
    if (e_max == 0) e_max <- 0.001
    w <- esw / e_max
    if (w > 1) {
      w <- 1
      delta <- esw - e_max
      if (delta < 0) esw <- e_max
    }
    if (esw < 0) esw <- 0
    r_ecl <- (1 / (fcl * hc) + r_cl) / (lr * i_m)
    ediff <- (1 - w) * (p_v_sk - vpa) / r_ecl
    evap <- -(ediff + esw)

    r_bare <- a_r_eff * (1 - f_a_cl) * e_skin * sbc *
      ((tr + 273.15)^4 - (t_sk + 273.15)^4) / a_dubois
    r_clo <- f_eff * a_clo * e_clo * sbc *
      ((tr + 273.15)^4 - (t_clo + 273.15)^4) / a_dubois
    r_sum <- r_clo + r_bare

    c_bare <- hc * (tdb - t_sk) * a_dubois * (1 - f_a_cl) / a_dubois
    c_clo <- hc * (tdb - t_clo) * a_clo / a_dubois
    csum <- c_clo + c_bare

    e_bal_vec[1] <- h + ere - (vasomotricity(t_cr, t_sk)$m_blood / 3600 * cb + 5.28) * (t_cr - t_sk)
    e_bal_vec[2] <- r_bare + c_bare + evap + (vasomotricity(t_cr, t_sk)$m_blood / 3600 * cb + 5.28) * (t_cr - t_sk) - htcl * (t_sk - t_clo)
    e_bal_vec[3] <- c_clo + r_clo + htcl * (t_sk - t_clo)
    e_bal_scal <- h + ere + r_sum + csum + evap

    if (actual_environment) {
      return(e_bal_vec)
    } else {
      return(e_bal_scal)
    }
  }

  pet_fc <- function(t_stable, tdb, tr) {
    f <- function(tx) {
      solvePet(t_stable, tx, tx, v = 0.1, rh = 50, met = 80, clo = 0.9, actual_environment = FALSE)
    }

    # Initial guess for the PET calculation based on the stable clothing temperature
    pet_guess <- t_stable[3] # start with the clothing temperature

    # Solve for the PET value where the function f(tx) returns 0
    result <- nleqslv(pet_guess, f)
    return(round(result$x, 2))
  }

  # Initial guess for solving temperatures
  t_guess <- c(36.7, 34, 0.5 * (tdb + tr))
  # Solve for stable temperatures
  t_stable <- nleqslv(t_guess, function(x) solvePet(x, tdb, tr, v, rh, met, clo, TRUE))

  # Calculate PET using the stable temperatures
  pet <- pet_fc(t_stable$x, tdb, tr)

  pet
}