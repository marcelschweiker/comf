#' Determines the adaptive thermal comfort based on EN 16798-1 2019. You can use this function to 
#' calculate if your conditions are within the EN adaptive thermal comfort region.
#' 
#' @param ta float, int, or array-like dry bulb air temperature, default in [°C] in [°F] if `units` = 'IP'
#' @param tr float, int, or array-like mean radiant temperature, default in [°C] in [°F] if `units` = 'IP'
#' @param t_running_mean float, int, or array-like running mean temperature, default in [°C] in [°C] in [°F] if `units` = 'IP'
#' @param v float, int, or array-like air speed, default in [m/s] in [fps] if `units` = 'IP'
#' @details Indoor operative temperature correction is applicable for buildings equipped 
#' with fans or personal systems providing building occupants with personal control over air speed at occupant level.
#' For operative temperatures above 25°C the comfort zone upper limit can be increased by 
#' 1.2 °C (0.6 < v < 0.9 m/s), 1.8 °C (0.9 < v < 1.2 m/s), 2.2 °C ( v > 1.2 m/s)
#' limit_inputs : boolean default True. By default, if the inputs are outsude the standard applicability 
#' limits the function returns nan. If False returns pmv and ppd values even if input values are 
#' outside the applicability limits of the model.
#' @return 
#' tmp_cmf : float, int, or array-like Comfort temperature at that specific running mean temperature, default in [°C] or in [°F]
#' acceptability_cat_i : bol or array-like. If the indoor conditions comply with comfort category I
#' acceptability_cat_ii : bol or array-like. If the indoor conditions comply with comfort category II
#' acceptability_cat_iii : bol or array-like. If the indoor conditions comply with comfort category III
#' tmp_cmf_cat_i_up : float, int, or array-like. Upper acceptable comfort temperature for category I, default in [°C] or in [°F]
#' tmp_cmf_cat_ii_up : float, int, or array-like. Upper acceptable comfort temperature for category II, default in [°C] or in [°F]
#' tmp_cmf_cat_iii_up : float, int, or array-like. Upper acceptable comfort temperature for category III, default in [°C] or in [°F]
#' tmp_cmf_cat_i_low : float, int, or array-like. Lower acceptable comfort temperature for category I, default in [°C] or in [°F]
#' tmp_cmf_cat_ii_low : float, int, or array-like. Lower acceptable comfort temperature for category II, default in [°C] or in [°F]
#' tmp_cmf_cat_iii_low : float or array-like. Lower acceptable comfort temperature for category III, default in [°C] or in [°F]
#' @references EN 16798-1 2019
#' @examples
#' calcAdaptiveEn(ta=25, tr=25, t_running_mean=20, v=0.1)
#' calcAdaptiveEn(ta=77, tr=77, t_running_mean=68, v=0.3, units='ip')
#' calcAdaptiveEn(ta=25, tr=25, t_running_mean=9, v=0.1)
#' @author Code implemented into R by Yiqing Zhang.
#' @export

# calcAdaptiveEn <- function(ta, tr, t_running_mean, v, units = "SI", limit_inputs = TRUE) {
#   # Check if inputs are numeric or lists of numeric values
#   if (!is.numeric(ta) && !is.list(ta)) {
#     stop("ta must be numeric or a list of numeric values.")
#   }
#   if (is.list(ta)) {
#     ta <- unlist(ta)
#     if (!is.numeric(ta)) {
#       stop("Elements of ta list must be numeric values.")
#     }
#   }
  
#   if (!is.numeric(tr) && !is.list(tr)) {
#     stop("tr must be numeric or a list of numeric values.")
#   }
#   if (is.list(tr)) {
#     tr <- unlist(tr)
#     if (!is.numeric(tr)) {
#       stop("Elements of tr list must be numeric values.")
#     }
#   }
  
#   if (!is.numeric(t_running_mean) && !is.list(t_running_mean)) {
#     stop("t_running_mean must be numeric or a list of numeric values.")
#   }
#   if (is.list(t_running_mean)) {
#     t_running_mean <- unlist(t_running_mean)
#     if (!is.numeric(t_running_mean)) {
#       stop("Elements of t_running_mean list must be numeric values.")
#     }
#   }
  
#   if (!is.numeric(v) && !is.list(v)) {
#     stop("v must be numeric or a list of numeric values.")
#   }
#   if (is.list(v)) {
#     v <- unlist(v)
#     if (!is.numeric(v)) {
#       stop("Elements of v list must be numeric values.")
#     }
#   }
  
#   # Ensure that inputs are vectors of the same length
#   max_length <- max(length(ta), length(tr), length(t_running_mean), length(v))
#   ta <- rep(ta, length.out = max_length)
#   tr <- rep(tr, length.out = max_length)
#   t_running_mean <- rep(t_running_mean, length.out = max_length)
#   v <- rep(v, length.out = max_length)
  
#   # Validate the units
#   valid_units <- c("SI", "IP")
#   if (!(toupper(units) %in% valid_units)) {
#     stop(paste("Invalid unit:", units, ". Supported units are", toString(valid_units), "."))
#   }
  
#   # Convert units if necessary (from IP to SI)
#   if (tolower(units) == "ip") {
#     ta <- (ta - 32) * 5/9  # Convert Fahrenheit to Celsius
#     tr <- (tr - 32) * 5/9
#     t_running_mean <- (t_running_mean - 32) * 5/9
#     v <- v * 0.00508  # Convert ft/min to m/s
#   }
  
#   # Validate t_running_mean range
#   valid_range <- function(values, range) {
#     values_valid <- ifelse(values >= range[1] & values <= range[2], values, NA)
#     return(values_valid)
#   }
#   trm_valid <- valid_range(t_running_mean, c(10.0, 33.5))
  
#   # Compute operative temperature
#   t_o <- function(ta, tr, v, standard = "ISO") {
#     if (toupper(standard) == "ISO") {
#       to <- ifelse(v < 0.1, (ta + tr) / 2, (ta * (3.0 * sqrt(v)) + tr) / (1 + 3.0 * sqrt(v)))
#     } else if (toupper(standard) == "ASHRAE") {
#       to <- ifelse(v < 0.2, (ta + tr) / 2, (ta * (1.0 + 0.6 * v) + tr) / (2.0 + 0.6 * v))
#     } else {
#       stop("Invalid standard. Choose 'ISO' or 'ASHRAE'.")
#     }
#     return(to)
#   }
  
#   to <- t_o(ta, tr, v, standard = "ISO")
  
#   # Compute cooling effect (ce) of elevated air speed when to >= 25°C
#   ce <- ifelse((v >= 0.6) & (to >= 25.0), 999, 0)
#   ce <- ifelse((v < 0.9) & (ce == 999), 1.2, ce)
#   ce <- ifelse((v < 1.2) & (ce == 999), 1.8, ce)
#   ce <- ifelse((ce == 999), 2.2, ce)
  
#   # Compute t_cmf
#   t_cmf <- 0.33 * t_running_mean + 18.8
  
#   if (limit_inputs) {
#     t_cmf <- ifelse(!is.na(trm_valid), t_cmf, NA)
#   }
  
#   # Compute temperature limits
#   t_cmf_i_lower <- t_cmf - 3.0
#   t_cmf_ii_lower <- t_cmf - 4.0
#   t_cmf_iii_lower <- t_cmf - 5.0
#   t_cmf_i_upper <- t_cmf + 2.0 + ce
#   t_cmf_ii_upper <- t_cmf + 3.0 + ce
#   t_cmf_iii_upper <- t_cmf + 4.0 + ce
  
#   # Compute acceptability
#   acceptability_i <- (t_cmf_i_lower <= to) & (to <= t_cmf_i_upper)
#   acceptability_ii <- (t_cmf_ii_lower <= to) & (to <= t_cmf_ii_upper)
#   acceptability_iii <- (t_cmf_iii_lower <= to) & (to <= t_cmf_iii_upper)
  
#   # Convert units back to IP if necessary
#   if (tolower(units) == "ip") {
#     t_cmf <- t_cmf * 9/5 + 32
#     t_cmf_i_upper <- t_cmf_i_upper * 9/5 + 32
#     t_cmf_ii_upper <- t_cmf_ii_upper * 9/5 + 32
#     t_cmf_iii_upper <- t_cmf_iii_upper * 9/5 + 32
#     t_cmf_i_lower <- t_cmf_i_lower * 9/5 + 32
#     t_cmf_ii_lower <- t_cmf_ii_lower * 9/5 + 32
#     t_cmf_iii_lower <- t_cmf_iii_lower * 9/5 + 32
#   }
  
#   # Assemble results
#   results <- list(
#     tmp_cmf = round(t_cmf, 1),
#     acceptability_cat_i = acceptability_i,
#     acceptability_cat_ii = acceptability_ii,
#     acceptability_cat_iii = acceptability_iii,
#     tmp_cmf_cat_i_up = round(t_cmf_i_upper, 1),
#     tmp_cmf_cat_ii_up = round(t_cmf_ii_upper, 1),
#     tmp_cmf_cat_iii_up = round(t_cmf_iii_upper, 1),
#     tmp_cmf_cat_i_low = round(t_cmf_i_lower, 1),
#     tmp_cmf_cat_ii_low = round(t_cmf_ii_lower, 1),
#     tmp_cmf_cat_iii_low = round(t_cmf_iii_lower, 1)
#   )
  
#   return(results)
# }

calcAdaptiveEn <- function(ta, tr, t_running_mean, v, units = "SI", limit_inputs = TRUE) {
  # Ensure inputs are numeric vectors
  ta <- as.numeric(unlist(ta))
  tr <- as.numeric(unlist(tr))
  t_running_mean <- as.numeric(unlist(t_running_mean))
  v <- as.numeric(unlist(v))
  
  if (any(is.na(ta) | is.na(tr) | is.na(t_running_mean) | is.na(v))) {
    stop("Input contains invalid values.")
  }

  # Ensure inputs have the same length
  max_length <- max(length(ta), length(tr), length(t_running_mean), length(v))
  ta <- rep(ta, length.out = max_length)
  tr <- rep(tr, length.out = max_length)
  t_running_mean <- rep(t_running_mean, length.out = max_length)
  v <- rep(v, length.out = max_length)
  
  # Unit conversion, keep it similar to Python
  if (tolower(units) == "ip") {
    ta <- (ta - 32) * 5 / 9
    tr <- (tr - 32) * 5 / 9
    t_running_mean <- (t_running_mean - 32) * 5 / 9
    v <- v * 0.00508
  }
  
  # Validate t_running_mean range (similar to valid_range in Python)
  trm_valid <- ifelse(t_running_mean >= 10 & t_running_mean <= 33.5, TRUE, FALSE)
  
  # Operative temperature calculation (replace "ISO" standard logic as needed)
  to <- ifelse(v < 0.1, (ta + tr) / 2, (ta * (3 * sqrt(v)) + tr) / (1 + 3 * sqrt(v)))
  
  # Cooling effect (ce) calculation, same as Python
  ce <- ifelse(v >= 0.6 & to >= 25, 999, 0)
  ce <- ifelse(v < 0.9 & ce == 999, 1.2, ce)
  ce <- ifelse(v < 1.2 & ce == 999, 1.8, ce)
  ce <- ifelse(ce == 999, 2.2, ce)
  
  # Calculate t_cmf
  t_cmf <- 0.33 * t_running_mean + 18.8
  if (limit_inputs) {
    t_cmf <- ifelse(trm_valid, t_cmf, NA)
  }
  
  # Calculate upper and lower comfort temperature limits
  t_cmf_i_lower <- t_cmf - 3.0
  t_cmf_ii_lower <- t_cmf - 4.0
  t_cmf_iii_lower <- t_cmf - 5.0
  t_cmf_i_upper <- t_cmf + 2.0 + ce
  t_cmf_ii_upper <- t_cmf + 3.0 + ce
  t_cmf_iii_upper <- t_cmf + 4.0 + ce
  
  # Compute acceptability based on categories
  acceptability_i <- (t_cmf_i_lower <= to) & (to <= t_cmf_i_upper)
  acceptability_ii <- (t_cmf_ii_lower <= to) & (to <= t_cmf_ii_upper)
  acceptability_iii <- (t_cmf_iii_lower <= to) & (to <= t_cmf_iii_upper)
  
  # Convert units back to IP if necessary
  if (tolower(units) == "ip") {
    t_cmf <- t_cmf * 9/5 + 32
    t_cmf_i_upper <- t_cmf_i_upper * 9/5 + 32
    t_cmf_ii_upper <- t_cmf_ii_upper * 9/5 + 32
    t_cmf_iii_upper <- t_cmf_iii_upper * 9/5 + 32
    t_cmf_i_lower <- t_cmf_i_lower * 9/5 + 32
    t_cmf_ii_lower <- t_cmf_ii_lower * 9/5 + 32
    t_cmf_iii_lower <- t_cmf_iii_lower * 9/5 + 32
  }
  
  # Assemble results into a list
  results <- list(
    tmp_cmf = round(t_cmf, 1),
    acceptability_cat_i = acceptability_i,
    acceptability_cat_ii = acceptability_ii,
    acceptability_cat_iii = acceptability_iii,
    tmp_cmf_cat_i_up = round(t_cmf_i_upper, 1),
    tmp_cmf_cat_ii_up = round(t_cmf_ii_upper, 1),
    tmp_cmf_cat_iii_up = round(t_cmf_iii_upper, 1),
    tmp_cmf_cat_i_low = round(t_cmf_i_lower, 1),
    tmp_cmf_cat_ii_low = round(t_cmf_ii_lower, 1),
    tmp_cmf_cat_iii_low = round(t_cmf_iii_lower, 1)
  )
  
  return(results)
}
