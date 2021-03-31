calcConvective <- function(posture="standing", va=0.1, ta=28.8, tsk=34.0){

# Natural convection
if (tolower(posture) == "standing"){
  # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
  hc_natural = c(
    4.48, 4.48, 2.97, 2.91, 2.85,
    3.61, 3.55, 3.67, 3.61, 3.55, 3.67,
    2.80, 2.04, 2.04, 2.80, 2.04, 2.04)}
else if (tolower(posture) == "sitting"  || tolower(posture) == "sedentary"){
    # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
    hc_natural = c(
      4.75, 4.75, 3.12, 2.48, 1.84,
      3.76, 3.62, 2.06, 3.76, 3.62, 2.06,
      2.98, 2.98, 2.62, 2.98, 2.98, 2.62)}

else if (tolower(posture) == "lying" || tolower(posture) == "supine"){
    # Kurazumi et al., 2008, https://doi.org/10.20718/jjpa.13.1_17
    # The values are applied under cold environment.
    hc_a = c(
      1.105, 1.105, 1.211, 1.211, 1.211,
      0.913, 2.081, 2.178, 0.913, 2.081, 2.178,
      0.945, 0.385, 0.200, 0.945, 0.385, 0.200)
    hc_b = c(
    0.345, 0.345, 0.046, 0.046, 0.046,
    0.373, 0.850, 0.297, 0.373, 0.850, 0.297,
    0.447, 0.580, 0.966, 0.447, 0.580, 0.966)
    hc_natural = hc_a * (abs(ta - tsk) ^ hc_b)
}
  # Forced convection
  # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
  hc_a = c(
    15.0, 15.0, 11.0, 17.0, 13.0,
    17.0, 17.0, 20.0, 17.0, 17.0, 20.0,
    14.0, 15.8, 15.1, 14.0, 15.8, 15.1)
  hc_b = c(
    0.62, 0.62, 0.67, 0.49, 0.60,
    0.59, 0.61, 0.60, 0.59, 0.61, 0.60,
    0.61, 0.74, 0.62, 0.61, 0.74, 0.62)
  hc_forced = hc_a * (va ^ hc_b)

  # Select natural or forced hc.
  # If local va is under 0.2 m/s, the hc value is natural.
  if (va < 0.2){
    hc = hc_natural
  }
  else {
    hc = hc_forced
  }
  return(hc)  # hc [W/K.m2)]
}
calcConvective(posture="sitting", va=0.1, ta=28.8, tsk=34.0)
