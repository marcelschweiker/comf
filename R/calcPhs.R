#' Calculates the Predicted Heat Strain (PHS) based on ISO 7933:2004 Standard
#'
#' The PHS model predicts:
#' - Heat by respiratory convection and evaporation
#' - Steady-state mean skin temperature and instantaneous skin temperature
#' - Heat accumulation associated with metabolic rate
#' - Maximum evaporative heat flow at the skin surface
#' - Predicted sweat rate and evaporative heat flow
#' - Rectal temperature
#'
#' @param tdb float or numeric vector. Dry bulb air temperature, default in Celsius
#' @param tr float or numeric vector. Mean radiant temperature, default in Celsius
#' @param v float or numeric vector. Air speed, default in [m/s]
#' @param rh float or numeric vector. Relative humidity, [%]
#' @param met float or numeric vector. Metabolic rate, [W/m2]
#' @param clo float or numeric vector. Clothing insulation, [clo]
#' @param posture integer. Numeric value representing the posture of the person
#' (1 = sitting, 2 = standing, 3 = crouching)
#' @param wme float or numeric vector. External work, [W/m2], default 0
#'
#' @param limit_inputs logical, default TRUE. If TRUE, returns NA when inputs are
#' outside the standard applicability limits (15 < tdb < 50 Celsius, 0 < tr < 60 Celsius,
#' 0 < vr < 3 [m/s], 100 < met < 450 [met], 0.1 < clo < 1).
#' @param i_mst float, default 0.38. Static moisture permeability index, [dimensionless]
#' @param a_p float, default 0.54. Fraction of body surface covered by reflective clothing, [dimensionless]
#' @param drink integer, default 1. Whether workers can drink freely (1 = yes, 0 = no)
#' @param weight float, default 75. Body weight, [kg]
#' @param height float, default 1.8. Height, [m]
#' @param walk_sp float, default 0. Walking speed, [m/s]
#' @param theta float, default 0. Angle between walking direction and wind direction, [Celsius]
#' @param acclimatized integer, default 100. 100 if acclimatized subject, 0 otherwise
#' @param duration integer, default 480. Duration of the work sequence, [minutes]
#' @param f_r float, default 0.97. Emissivity of reflective clothing, [dimensionless]
#' @param t_sk float, default 34.1. Mean skin temperature when worker starts working, Celsius
#' @param t_cr float, default 36.8. Mean core temperature when worker starts working, Celsius
#' @param t_re float, default FALSE. Mean rectal temperature when worker starts working, Celsius
#' @param t_cr_eq float, default FALSE. Mean core temperature as a function of the metabolic rate when worker starts working,
#' @param t_sk_t_cr_wg fraction of the body mass at the skin temperature
#' @param sweat_rate float, default 0. Initial sweat rate
#'
#' @return A list containing the following:
#' - `t_re`: Rectal temperature,
#' - `t_sk`: Skin temperature,
#' - `t_cr`: Core temperature, Celsius
#' - `t_cr_eq`: Core temperature as a function of metabolic rate, Celsius
#' - `t_sk_t_cr_wg`: Fraction of body mass at the skin temperature
#' - `d_lim_loss_50`: Maximum allowable exposure time for water loss (mean subject), [minutes]
#' - `d_lim_loss_95`: Maximum allowable exposure time for water loss (95% of working population), [minutes]
#' - `d_lim_t_re`: Maximum allowable exposure time for heat storage, [minutes]
#' - `water_loss_watt`: Maximum water loss in watts, [W]
#' - `water_loss`: Maximum water loss, [g]
#'
#' @examples
#' # Example usage of phs function
#' results <- phs(tdb = 40, tr = 40, rh = 33.85, v = 0.3, met = 150, clo = 0.5, posture = 2, wme = 0)
#' print(results)
#'
#' @export
calcPhs <- function(
    tdb,
    tr,
    v,
    rh,
    met,
    clo,
    posture,
    wme = 0,
    limit_inputs = TRUE,
    i_mst = 0.38,
    a_p = 0.54,
    drink = 1,
    weight = 75,
    height = 1.8,
    walk_sp = 0,
    theta = 0,
    acclimatized = 100,
    duration = 480,
    f_r = 0.97,
    t_sk = 34.1,
    t_cr = 36.8,
    t_re = FALSE,
    t_cr_eq = FALSE,
    t_sk_t_cr_wg = 0.3,
    sweat_rate = 0) {
  
  met <- met * 58.15
  wme <- wme * 58.15
  p_a <- p_sat(tdb) / 1000 * rh / 100

  if (acclimatized < 0 || acclimatized > 100) {
    stop("Acclimatized should be between 0 and 100")
  }

  if (!(drink %in% c(0, 1))) {
    stop("Drink should be 0 or 1")
  }

  if (weight <= 0 || weight > 1000) {
    stop("The weight of the person should be in kg and it cannot exceed 1000")
  }

  combined_drink_acc_weight <- drink * 1000000 + acclimatized * 1000 + weight

  if (isFALSE(t_re) || is.null(t_re)) {
    t_re <- t_cr
  }

  if (isFALSE(t_cr_eq) || is.null(t_cr_eq)) {
    t_cr_eq <- t_cr
  }

  const_t_eq <- exp(-1 / 10)
  const_t_sk <- exp(-1 / 3)
  const_sw <- exp(-1 / 10)

  drink <- as.integer(combined_drink_acc_weight / 1000000)
  acclimatized <- as.integer((combined_drink_acc_weight - drink * 1000000) / 1000)
  weight <- combined_drink_acc_weight - drink * 1000000 - acclimatized * 1000

  # DuBois body surface area [m2]
  a_dubois <- 0.202 * (weight^0.425) * (height^0.725)
  sp_heat <- 57.83 * weight / a_dubois # specific heat of the body
  d_lim_t_re <- 0 # maximum allowable exposure time for heat storage [min]
  d_lim_loss_50 <- 0 # maximum allowable exposure time for water loss, mean subject [min]
  d_lim_loss_95 <- 0 # maximum allowable exposure time for water loss, 95% of working population [min]
  d_max_50 <- 0.075 * weight * 1000 # max water loss for mean subject [g]
  d_max_95 <- 0.05 * weight * 1000 # max water loss for 95% of working population [g]
  sw_tot <- sweat_rate

  def_dir <- ifelse(theta != 0, 1, 0)
  def_speed <- ifelse(walk_sp == 0, 0, 1)

  # Radiating area dubois
  a_r_du <- ifelse(posture == 2, 0.77, ifelse(posture == 3, 0.67, 0.7))

  # Max sweat rate based on metabolic rate
  sw_max <- (met - 32) * a_dubois
  sw_max <- ifelse(sw_max > 400, 400, ifelse(sw_max < 250, 250, sw_max))
  sw_max <- ifelse(acclimatized >= 50, sw_max * 1.25, sw_max)

  w_max <- ifelse(acclimatized < 50, 0.85, 1)

  # Static clothing insulation
  i_cl_st <- clo * 0.155
  fcl <- 1 + 0.3 * clo
  i_a_st <- 0.111
  i_tot_st <- i_cl_st + i_a_st / fcl

  # Correct for walking speed and direction
  if (def_speed > 0) {
    if (def_dir == 1) {
      v_r <- abs(v - walk_sp * cos(pi * theta / 180))
    } else {
      v_r <- ifelse(v < walk_sp, walk_sp, v)
    }
  } else {
    walk_sp <- 0.0052 * (met - 58)
    walk_sp <- ifelse(walk_sp > 0.7, 0.7, walk_sp)
    v_r <- v
  }

  v_ux <- ifelse(v_r > 3, 3, v_r)
  w_a_ux <- ifelse(walk_sp > 1.5, 1.5, walk_sp)

  corr_cl <- 1.044 * exp((0.066 * v_ux - 0.398) * v_ux + (0.094 * w_a_ux - 0.378) * w_a_ux)
  corr_cl <- ifelse(corr_cl > 1, 1, corr_cl)

  corr_ia <- exp((0.047 * v_r - 0.472) * v_r + (0.117 * w_a_ux - 0.342) * w_a_ux)
  corr_ia <- ifelse(corr_ia > 1, 1, corr_ia)

  corr_tot <- ifelse(clo <= 0.6, ((0.6 - clo) * corr_ia + clo * corr_cl) / 0.6, corr_cl)
  i_tot_dyn <- i_tot_st * corr_tot
  i_a_dyn <- corr_ia * i_a_st
  i_cl_dyn <- i_tot_dyn - i_a_dyn / fcl

  corr_e <- (2.6 * corr_tot - 6.5) * corr_tot + 4.9
  im_dyn <- i_mst * corr_e
  im_dyn <- ifelse(im_dyn > 0.9, 0.9, im_dyn)

  r_t_dyn <- i_tot_dyn / im_dyn / 16.7
  t_exp <- 28.56 + 0.115 * tdb + 0.641 * p_a

  c_res <- 0.001516 * met * (t_exp - tdb)
  e_res <- 0.00127 * met * (59.34 + 0.53 * tdb - 11.63 * p_a)

  z <- ifelse(v_r > 1, 8.7 * v_r^0.6, 3.5 + 5.2 * v_r)
  hc_dyn <- max(2.38 * abs(t_sk - tdb)^0.25, z)

  aux_r <- 5.67e-08 * a_r_du
  f_cl_r <- (1 - a_p) * 0.97 + a_p * f_r

  for (time in seq_len(duration)) {
    t_sk0 <- t_sk
    t_re0 <- t_re
    t_cr0 <- t_cr
    t_cr_eq0 <- t_cr_eq
    t_sk_t_cr_wg0 <- t_sk_t_cr_wg

    t_cr_eq_m <- 0.0036 * met + 36.6
    t_cr_eq <- t_cr_eq0 * const_t_eq + t_cr_eq_m * (1 - const_t_eq)
    d_stored_eq <- sp_heat * (t_cr_eq - t_cr_eq0) * (1 - t_sk_t_cr_wg0)

    t_sk_eq_cl <- 12.165 + 0.02017 * tdb + 0.04361 * tr + 0.19354 * p_a - 0.25315 * v + 0.005346 * met + 0.51274 * t_re
    t_sk_eq_nu <- 7.191 + 0.064 * tdb + 0.061 * tr + 0.198 * p_a - 0.348 * v + 0.616 * t_re

    t_sk_eq <- ifelse(clo >= 0.6, t_sk_eq_cl, ifelse(clo <= 0.2, t_sk_eq_nu, t_sk_eq_nu + 2.5 * (t_sk_eq_cl - t_sk_eq_nu) * (clo - 0.2)))

    t_sk <- t_sk0 * const_t_sk + t_sk_eq * (1 - const_t_sk)
    p_sk <- 0.6105 * exp(17.27 * t_sk / (t_sk + 237.3))
    t_cl <- tr + 0.1

    repeat {
      h_r <- f_cl_r * aux_r * ((t_cl + 273)^4 - (tr + 273)^4) / (t_cl - tr)
      t_cl_new <- (fcl * (hc_dyn * tdb + h_r * tr) + t_sk / i_cl_dyn) / (fcl * (hc_dyn + h_r) + 1 / i_cl_dyn)
      if (abs(t_cl - t_cl_new) <= 0.001) break
      t_cl <- (t_cl + t_cl_new) / 2
    }

    convection <- fcl * hc_dyn * (t_cl - tdb)
    radiation <- fcl * h_r * (t_cl - tr)
    e_max <- (p_sk - p_a) / r_t_dyn
    e_max <- ifelse(e_max == 0, 0.001, e_max)
    e_req <- met - d_stored_eq - wme - c_res - e_res - convection - radiation
    w_req <- e_req / e_max

    if (e_req <= 0) {
      e_req <- 0
      sw_req <- 0  # required sweat rate [W/m2]
    } else if (e_max <= 0) {
      e_max <- 0
      sw_req <- sw_max
    } else if (w_req >= 1.7) {
      sw_req <- sw_max
    } else {
      e_v_eff <- 1 - (w_req^2) / 2
      if (w_req > 1) {
        e_v_eff <- (2 - w_req)^2 / 2
      }
      sw_req <- e_req / e_v_eff
      if (sw_req > sw_max) {
        sw_req <- sw_max
      }
    }
    sweat_rate <- sweat_rate * const_sw + sw_req * (1 - const_sw)

    e_p <- ifelse(sweat_rate <= 0, 0, {
      k <- e_max / sweat_rate
      wp <- ifelse(k >= 0.5, -k + sqrt(k^2 + 2), 1)
      wp <- min(wp, w_max)
      wp * e_max
    })

    d_storage <- e_req - e_p + d_stored_eq
    t_cr_new <- t_cr0

    repeat {
      t_sk_t_cr_wg <- 0.3 - 0.09 * (t_cr_new - 36.8)
      t_sk_t_cr_wg <- ifelse(t_sk_t_cr_wg > 0.3, 0.3,
        ifelse(t_sk_t_cr_wg < 0.1, 0.1, t_sk_t_cr_wg)
      )

      t_cr <- (d_storage / sp_heat + t_sk0 * t_sk_t_cr_wg0 / 2 - t_sk * t_sk_t_cr_wg / 2)
      t_cr <- (t_cr + t_cr0 * (1 - t_sk_t_cr_wg0 / 2)) / (1 - t_sk_t_cr_wg / 2)

      if (abs(t_cr - t_cr_new) <= 0.001) break
      t_cr_new <- (t_cr_new + t_cr) / 2
    }

    t_re <- t_re0 + (2 * t_cr - 1.962 * t_re0 - 1.31) / 9

    if (d_lim_t_re == 0 && t_re >= 38) {
      d_lim_t_re <- time
    }

    sw_tot <- sw_tot + sweat_rate + e_res
    sw_tot_g <- sw_tot * 2.67 * a_dubois / 1.8 / 60
    if (d_lim_loss_50 == 0 && sw_tot_g >= d_max_50) {
      d_lim_loss_50 <- time
    }

    if (d_lim_loss_95 == 0 && sw_tot_g >= d_max_95) {
      d_lim_loss_95 <- time
    }

    if (drink == 0) {
      d_lim_loss_95 <- d_lim_loss_95 * 0.6
      d_lim_loss_50 <- d_lim_loss_95
    }
  }

  if (d_lim_loss_50 == 0) d_lim_loss_50 <- duration
  if (d_lim_loss_95 == 0) d_lim_loss_95 <- duration
  if (d_lim_t_re == 0) d_lim_t_re <- duration

  return(
    list(
      t_re = t_re,
      t_sk = t_sk,
      t_cr = t_cr,
      t_cr_eq = t_cr_eq,
      t_sk_t_cr_wg = t_sk_t_cr_wg,
      water_loss_watt = sweat_rate,
      water_loss = sw_tot_g,
      d_lim_loss_50 = d_lim_loss_50,
      d_lim_loss_95 = d_lim_loss_95,
      d_lim_t_re = d_lim_t_re
    )
  )
}
