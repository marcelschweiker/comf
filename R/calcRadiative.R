calcRadiative <- function(posture="standing"){

if (tolower(posture) == "standing"){
  # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
  hr = c(
    4.89, 4.89, 4.32, 4.09, 4.32,
    4.55, 4.43, 4.21, 4.55, 4.43, 4.21,
    4.77, 5.34, 6.14, 4.77, 5.34, 6.14)}
else if (tolower(posture) == "sitting"  || tolower(posture) == "sedentary"){
  # Ichihara et al., 1997, https://doi.org/10.3130/aija.62.45_5
  hr = c(
    4.96, 4.96, 3.99, 4.64, 4.21,
    4.96, 4.21, 4.74, 4.96, 4.21, 4.74,
    4.10, 4.74, 6.36, 4.10, 4.74, 6.36)}

else if (tolower(posture) == "lying" || tolower(posture) == "supine"){

  # Kurazumi et al., 2008, https://doi.org/10.20718/jjpa.13.1_17
  hr = c(
    5.475, 5.475, 3.463, 3.463, 3.463,
    4.249, 4.835, 4.119, 4.249, 4.835, 4.119,
    4.440, 5.547, 6.085, 4.440, 5.547, 6.085)
}

  return(hr)
}
