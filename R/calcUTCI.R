# Functions return:
#
# UTCI - Universal Thermal Climate Index
#
# File contains 2 functions:
#   - calcUTCI(ta, tr, vel, rh)
#       returns UTCI
#
# v2.0 done by Shaomi Rahman

calcUTCI <- function(ta, tr, vel, rh) {
  
  #validate the inputs and prints an error message for invalid inputs
  validate_UTCI(ta, tr, vel, rh)
  
  #calculate utci value from function
  utci_value = utci_approx(ta, tr, vel, rh)
  
  #check if utci value is within acceptable range
  utci_range = getUtciRange()
  check_range(utci_value, round(utci_range[1],3), round(utci_range[2],3))
  
  #return the value
  round(utci_value,1)
}


#function for calculating UTCI Value
utci_approx <- function(ta, tr, vel, rh) {
  ehPa = es(ta) * (rh / 100.0)
  delta_t_tr = tr - ta
  
  # convelert velapour pressure to kPa
  Pa = ehPa / 10.0
  
  #calculation of the utci value
  utci = (
    ta
    + (0.607562052)
    + (-0.0227712343) * ta
    + (8.06470249 * (10 ** (-4))) * ta * ta
    + (-1.54271372 * (10 ** (-4))) * ta * ta * ta
    + (-3.24651735 * (10 ** (-6))) * ta * ta * ta * ta
    + (7.32602852 * (10 ** (-8))) * ta * ta * ta * ta * ta
    + (1.35959073 * (10 ** (-9))) * ta * ta * ta * ta * ta * ta
    + (-2.25836520) * vel
    + (0.0880326035) * ta * vel
    + (0.00216844454) * ta * ta * vel
    + (-1.53347087 * (10 ** (-5))) * ta * ta * ta * vel
    + (-5.72983704 * (10 ** (-7))) * ta * ta * ta * ta * vel
    + (-2.55090145 * (10 ** (-9))) * ta * ta * ta * ta * ta * vel
    + (-0.751269505) * vel * vel
    + (-0.00408350271) * ta * vel * vel
    + (-5.21670675 * (10 ** (-5))) * ta * ta * vel * vel
    + (1.94544667 * (10 ** (-6))) * ta * ta * ta * vel * vel
    + (1.14099531 * (10 ** (-8))) * ta * ta * ta * ta * vel * vel
    + (0.158137256) * vel * vel * vel
    + (-6.57263143 * (10 ** (-5))) * ta * vel * vel * vel
    + (2.22697524 * (10 ** (-7))) * ta * ta * vel * vel * vel
    + (-4.16117031 * (10 ** (-8))) * ta * ta * ta * vel * vel * vel
    + (-0.0127762753) * vel * vel * vel * vel
    + (9.66891875 * (10 ** (-6))) * ta * vel * vel * vel * vel
    + (2.52785852 * (10 ** (-9))) * ta * ta * vel * vel * vel * vel
    + (4.56306672 * (10 ** (-4))) * vel * vel * vel * vel * vel
    + (-1.74202546 * (10 ** (-7))) * ta * vel * vel * vel * vel * vel
    + (-5.91491269 * (10 ** (-6))) * vel * vel * vel * vel * vel * vel
    + (0.398374029) * delta_t_tr
    + (1.83945314 * (10 ** (-4))) * ta * delta_t_tr
    + (-1.73754510 * (10 ** (-4))) * ta * ta * delta_t_tr
    + (-7.60781159 * (10 ** (-7))) * ta * ta * ta * delta_t_tr
    + (3.77830287 * (10 ** (-8))) * ta * ta * ta * ta * delta_t_tr
    + (5.43079673 * (10 ** (-10))) * ta * ta * ta * ta * ta * delta_t_tr
    + (-0.0200518269) * vel * delta_t_tr
    + (8.92859837 * (10 ** (-4))) * ta * vel * delta_t_tr
    + (3.45433048 * (10 ** (-6))) * ta * ta * vel * delta_t_tr
    + (-3.77925774 * (10 ** (-7))) * ta * ta * ta * vel * delta_t_tr
    + (-1.69699377 * (10 ** (-9))) * ta * ta * ta * ta * vel * delta_t_tr
    + (1.69992415 * (10 ** (-4))) * vel * vel * delta_t_tr
    + (-4.99204314 * (10 ** (-5))) * ta * vel * vel * delta_t_tr
    + (2.47417178 * (10 ** (-7))) * ta * ta * vel * vel * delta_t_tr
    + (1.07596466 * (10 ** (-8))) * ta * ta * ta * vel * vel * delta_t_tr
    + (8.49242932 * (10 ** (-5))) * vel * vel * vel * delta_t_tr
    + (1.35191328 * (10 ** (-6))) * ta * vel * vel * vel * delta_t_tr
    + (-6.21531254 * (10 ** (-9))) * ta * ta * vel * vel * vel * delta_t_tr
    + (-4.99410301 * (10 ** (-6))) * vel * vel * vel * vel * delta_t_tr
    + (-1.89489258 * (10 ** (-8))) * ta * vel * vel * vel * vel * delta_t_tr
    + (8.15300114 * (10 ** (-8))) * vel * vel * vel * vel * vel * delta_t_tr
    + (7.55043090 * (10 ** (-4))) * delta_t_tr * delta_t_tr
    + (-5.65095215 * (10 ** (-5))) * ta * delta_t_tr * delta_t_tr
    + (-4.52166564 * (10 ** (-7))) * ta * ta * delta_t_tr * delta_t_tr
    + (2.46688878 * (10 ** (-8))) * ta * ta * ta * delta_t_tr * delta_t_tr
    + (2.42674348 * (10 ** (-10))) * ta * ta * ta * ta * delta_t_tr * delta_t_tr
    + (1.54547250 * (10 ** (-4))) * vel * delta_t_tr * delta_t_tr
    + (5.24110970 * (10 ** (-6))) * ta * vel * delta_t_tr * delta_t_tr
    + (-8.75874982 * (10 ** (-8))) * ta * ta * vel * delta_t_tr * delta_t_tr
    + (-1.50743064 * (10 ** (-9))) * ta * ta * ta * vel * delta_t_tr * delta_t_tr
    + (-1.56236307 * (10 ** (-5))) * vel * vel * delta_t_tr * delta_t_tr
    + (-1.33895614 * (10 ** (-7))) * ta * vel * vel * delta_t_tr * delta_t_tr
    + (2.49709824 * (10 ** (-9))) * ta * ta * vel * vel * delta_t_tr * delta_t_tr
    + (6.51711721 * (10 ** (-7))) * vel * vel * vel * delta_t_tr * delta_t_tr
    + (1.94960053 * (10 ** (-9))) * ta * vel * vel * vel * delta_t_tr * delta_t_tr
    + (-1.00361113 * (10 ** (-8))) * vel * vel * vel * vel * delta_t_tr * delta_t_tr
    + (-1.21206673 * (10 ** (-5))) * delta_t_tr * delta_t_tr * delta_t_tr
    + (-2.18203660 * (10 ** (-7))) * ta * delta_t_tr * delta_t_tr * delta_t_tr
    + (7.51269482 * (10 ** (-9))) * ta * ta * delta_t_tr * delta_t_tr * delta_t_tr
    + (9.79063848 * (10 ** (-11)))
    * ta
    * ta
    * ta
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (1.25006734 * (10 ** (-6))) * vel * delta_t_tr * delta_t_tr * delta_t_tr
    + (-1.81584736 * (10 ** (-9))) * ta * vel * delta_t_tr * delta_t_tr * delta_t_tr
    + (-3.52197671 * (10 ** (-10)))
    * ta
    * ta
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (-3.36514630 * (10 ** (-8))) * vel * vel * delta_t_tr * delta_t_tr * delta_t_tr
    + (1.35908359 * (10 ** (-10)))
    * ta
    * vel
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (4.17032620 * (10 ** (-10)))
    * vel
    * vel
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (-1.30369025 * (10 ** (-9)))
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (4.13908461 * (10 ** (-10)))
    * ta
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (9.22652254 * (10 ** (-12)))
    * ta
    * ta
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (-5.08220384 * (10 ** (-9)))
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (-2.24730961 * (10 ** (-11)))
    * ta
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (1.17139133 * (10 ** (-10)))
    * vel
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (6.62154879 * (10 ** (-10)))
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (4.03863260 * (10 ** (-13)))
    * ta
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (1.95087203 * (10 ** (-12)))
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (-4.73602469 * (10 ** (-12)))
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    + (5.12733497) * Pa
    + (-0.312788561) * ta * Pa
    + (-0.0196701861) * ta * ta * Pa
    + (9.99690870 * (10 ** (-4))) * ta * ta * ta * Pa
    + (9.51738512 * (10 ** (-6))) * ta * ta * ta * ta * Pa
    + (-4.66426341 * (10 ** (-7))) * ta * ta * ta * ta * ta * Pa
    + (0.548050612) * vel * Pa
    + (-0.00330552823) * ta * vel * Pa
    + (-0.00164119440) * ta * ta * vel * Pa
    + (-5.16670694 * (10 ** (-6))) * ta * ta * ta * vel * Pa
    + (9.52692432 * (10 ** (-7))) * ta * ta * ta * ta * vel * Pa
    + (-0.0429223622) * vel * vel * Pa
    + (0.00500845667) * ta * vel * vel * Pa
    + (1.00601257 * (10 ** (-6))) * ta * ta * vel * vel * Pa
    + (-1.81748644 * (10 ** (-6))) * ta * ta * ta * vel * vel * Pa
    + (-1.25813502 * (10 ** (-3))) * vel * vel * vel * Pa
    + (-1.79330391 * (10 ** (-4))) * ta * vel * vel * vel * Pa
    + (2.34994441 * (10 ** (-6))) * ta * ta * vel * vel * vel * Pa
    + (1.29735808 * (10 ** (-4))) * vel * vel * vel * vel * Pa
    + (1.29064870 * (10 ** (-6))) * ta * vel * vel * vel * vel * Pa
    + (-2.28558686 * (10 ** (-6))) * vel * vel * vel * vel * vel * Pa
    + (-0.0369476348) * delta_t_tr * Pa
    + (0.00162325322) * ta * delta_t_tr * Pa
    + (-3.14279680 * (10 ** (-5))) * ta * ta * delta_t_tr * Pa
    + (2.59835559 * (10 ** (-6))) * ta * ta * ta * delta_t_tr * Pa
    + (-4.77136523 * (10 ** (-8))) * ta * ta * ta * ta * delta_t_tr * Pa
    + (8.64203390 * (10 ** (-3))) * vel * delta_t_tr * Pa
    + (-6.87405181 * (10 ** (-4))) * ta * vel * delta_t_tr * Pa
    + (-9.13863872 * (10 ** (-6))) * ta * ta * vel * delta_t_tr * Pa
    + (5.15916806 * (10 ** (-7))) * ta * ta * ta * vel * delta_t_tr * Pa
    + (-3.59217476 * (10 ** (-5))) * vel * vel * delta_t_tr * Pa
    + (3.28696511 * (10 ** (-5))) * ta * vel * vel * delta_t_tr * Pa
    + (-7.10542454 * (10 ** (-7))) * ta * ta * vel * vel * delta_t_tr * Pa
    + (-1.24382300 * (10 ** (-5))) * vel * vel * vel * delta_t_tr * Pa
    + (-7.38584400 * (10 ** (-9))) * ta * vel * vel * vel * delta_t_tr * Pa
    + (2.20609296 * (10 ** (-7))) * vel * vel * vel * vel * delta_t_tr * Pa
    + (-7.32469180 * (10 ** (-4))) * delta_t_tr * delta_t_tr * Pa
    + (-1.87381964 * (10 ** (-5))) * ta * delta_t_tr * delta_t_tr * Pa
    + (4.80925239 * (10 ** (-6))) * ta * ta * delta_t_tr * delta_t_tr * Pa
    + (-8.75492040 * (10 ** (-8))) * ta * ta * ta * delta_t_tr * delta_t_tr * Pa
    + (2.77862930 * (10 ** (-5))) * vel * delta_t_tr * delta_t_tr * Pa
    + (-5.06004592 * (10 ** (-6))) * ta * vel * delta_t_tr * delta_t_tr * Pa
    + (1.14325367 * (10 ** (-7))) * ta * ta * vel * delta_t_tr * delta_t_tr * Pa
    + (2.53016723 * (10 ** (-6))) * vel * vel * delta_t_tr * delta_t_tr * Pa
    + (-1.72857035 * (10 ** (-8))) * ta * vel * vel * delta_t_tr * delta_t_tr * Pa
    + (-3.95079398 * (10 ** (-8))) * vel * vel * vel * delta_t_tr * delta_t_tr * Pa
    + (-3.59413173 * (10 ** (-7))) * delta_t_tr * delta_t_tr * delta_t_tr * Pa
    + (7.04388046 * (10 ** (-7))) * ta * delta_t_tr * delta_t_tr * delta_t_tr * Pa
    + (-1.89309167 * (10 ** (-8)))
    * ta
    * ta
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    + (-4.79768731 * (10 ** (-7))) * vel * delta_t_tr * delta_t_tr * delta_t_tr * Pa
    + (7.96079978 * (10 ** (-9)))
    * ta
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    + (1.62897058 * (10 ** (-9)))
    * vel
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    + (3.94367674 * (10 ** (-8)))
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    + (-1.18566247 * (10 ** (-9)))
    * ta
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    + (3.34678041 * (10 ** (-10)))
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    + (-1.15606447 * (10 ** (-10)))
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    + (-2.80626406) * Pa * Pa
    + (0.548712484) * ta * Pa * Pa
    + (-0.00399428410) * ta * ta * Pa * Pa
    + (-9.54009191 * (10 ** (-4))) * ta * ta * ta * Pa * Pa
    + (1.93090978 * (10 ** (-5))) * ta * ta * ta * ta * Pa * Pa
    + (-0.308806365) * vel * Pa * Pa
    + (0.0116952364) * ta * vel * Pa * Pa
    + (4.95271903 * (10 ** (-4))) * ta * ta * vel * Pa * Pa
    + (-1.90710882 * (10 ** (-5))) * ta * ta * ta * vel * Pa * Pa
    + (0.00210787756) * vel * vel * Pa * Pa
    + (-6.98445738 * (10 ** (-4))) * ta * vel * vel * Pa * Pa
    + (2.30109073 * (10 ** (-5))) * ta * ta * vel * vel * Pa * Pa
    + (4.17856590 * (10 ** (-4))) * vel * vel * vel * Pa * Pa
    + (-1.27043871 * (10 ** (-5))) * ta * vel * vel * vel * Pa * Pa
    + (-3.04620472 * (10 ** (-6))) * vel * vel * vel * vel * Pa * Pa
    + (0.0514507424) * delta_t_tr * Pa * Pa
    + (-0.00432510997) * ta * delta_t_tr * Pa * Pa
    + (8.99281156 * (10 ** (-5))) * ta * ta * delta_t_tr * Pa * Pa
    + (-7.14663943 * (10 ** (-7))) * ta * ta * ta * delta_t_tr * Pa * Pa
    + (-2.66016305 * (10 ** (-4))) * vel * delta_t_tr * Pa * Pa
    + (2.63789586 * (10 ** (-4))) * ta * vel * delta_t_tr * Pa * Pa
    + (-7.01199003 * (10 ** (-6))) * ta * ta * vel * delta_t_tr * Pa * Pa
    + (-1.06823306 * (10 ** (-4))) * vel * vel * delta_t_tr * Pa * Pa
    + (3.61341136 * (10 ** (-6))) * ta * vel * vel * delta_t_tr * Pa * Pa
    + (2.29748967 * (10 ** (-7))) * vel * vel * vel * delta_t_tr * Pa * Pa
    + (3.04788893 * (10 ** (-4))) * delta_t_tr * delta_t_tr * Pa * Pa
    + (-6.42070836 * (10 ** (-5))) * ta * delta_t_tr * delta_t_tr * Pa * Pa
    + (1.16257971 * (10 ** (-6))) * ta * ta * delta_t_tr * delta_t_tr * Pa * Pa
    + (7.68023384 * (10 ** (-6))) * vel * delta_t_tr * delta_t_tr * Pa * Pa
    + (-5.47446896 * (10 ** (-7))) * ta * vel * delta_t_tr * delta_t_tr * Pa * Pa
    + (-3.59937910 * (10 ** (-8))) * vel * vel * delta_t_tr * delta_t_tr * Pa * Pa
    + (-4.36497725 * (10 ** (-6))) * delta_t_tr * delta_t_tr * delta_t_tr * Pa * Pa
    + (1.68737969 * (10 ** (-7)))
    * ta
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    * Pa
    + (2.67489271 * (10 ** (-8)))
    * vel
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    * Pa
    + (3.23926897 * (10 ** (-9)))
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    * Pa
    + (-0.0353874123) * Pa * Pa * Pa
    + (-0.221201190) * ta * Pa * Pa * Pa
    + (0.0155126038) * ta * ta * Pa * Pa * Pa
    + (-2.63917279 * (10 ** (-4))) * ta * ta * ta * Pa * Pa * Pa
    + (0.0453433455) * vel * Pa * Pa * Pa
    + (-0.00432943862) * ta * vel * Pa * Pa * Pa
    + (1.45389826 * (10 ** (-4))) * ta * ta * vel * Pa * Pa * Pa
    + (2.17508610 * (10 ** (-4))) * vel * vel * Pa * Pa * Pa
    + (-6.66724702 * (10 ** (-5))) * ta * vel * vel * Pa * Pa * Pa
    + (3.33217140 * (10 ** (-5))) * vel * vel * vel * Pa * Pa * Pa
    + (-0.00226921615) * delta_t_tr * Pa * Pa * Pa
    + (3.80261982 * (10 ** (-4))) * ta * delta_t_tr * Pa * Pa * Pa
    + (-5.45314314 * (10 ** (-9))) * ta * ta * delta_t_tr * Pa * Pa * Pa
    + (-7.96355448 * (10 ** (-4))) * vel * delta_t_tr * Pa * Pa * Pa
    + (2.53458034 * (10 ** (-5))) * ta * vel * delta_t_tr * Pa * Pa * Pa
    + (-6.31223658 * (10 ** (-6))) * vel * vel * delta_t_tr * Pa * Pa * Pa
    + (3.02122035 * (10 ** (-4))) * delta_t_tr * delta_t_tr * Pa * Pa * Pa
    + (-4.77403547 * (10 ** (-6))) * ta * delta_t_tr * delta_t_tr * Pa * Pa * Pa
    + (1.73825715 * (10 ** (-6))) * vel * delta_t_tr * delta_t_tr * Pa * Pa * Pa
    + (-4.09087898 * (10 ** (-7)))
    * delta_t_tr
    * delta_t_tr
    * delta_t_tr
    * Pa
    * Pa
    * Pa
    + (0.614155345) * Pa * Pa * Pa * Pa
    + (-0.0616755931) * ta * Pa * Pa * Pa * Pa
    + (0.00133374846) * ta * ta * Pa * Pa * Pa * Pa
    + (0.00355375387) * vel * Pa * Pa * Pa * Pa
    + (-5.13027851 * (10 ** (-4))) * ta * vel * Pa * Pa * Pa * Pa
    + (1.02449757 * (10 ** (-4))) * vel * vel * Pa * Pa * Pa * Pa
    + (-0.00148526421) * delta_t_tr * Pa * Pa * Pa * Pa
    + (-4.11469183 * (10 ** (-5))) * ta * delta_t_tr * Pa * Pa * Pa * Pa
    + (-6.80434415 * (10 ** (-6))) * vel * delta_t_tr * Pa * Pa * Pa * Pa
    + (-9.77675906 * (10 ** (-6))) * delta_t_tr * delta_t_tr * Pa * Pa * Pa * Pa
    + (0.0882773108) * Pa * Pa * Pa * Pa * Pa
    + (-0.00301859306) * ta * Pa * Pa * Pa * Pa * Pa
    + (0.00104452989) * vel * Pa * Pa * Pa * Pa * Pa
    + (2.47090539 * (10 ** (-4))) * delta_t_tr * Pa * Pa * Pa * Pa * Pa
    + (0.00148348065) * Pa * Pa * Pa * Pa * Pa * Pa
  )
}

#function for calculating the es value
es <- function(ta) {
  g <- c(-2836.5744,
         -6028.076559,
         19.54263612,
         -0.02737830188,
         0.000016261698,
         (7.0229056 * (10 ** (-10))),
         (-1.8680009 * (10 ** (-13))))
  # converting air temparature in K
  tk <- ta + 273.15 
  es <- 2.7150305 * log1p(tk)
  for(i in seq_along(g)){
    es <- es + (g[i] * (tk ** (i - 3)))
  }
  # converting to hPa
  es = exp(es) * 0.01
  es
}

