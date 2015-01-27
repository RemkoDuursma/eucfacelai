
# Calculate diffuse transmittance, based on LAI, leaf angle distribution,
# following Campbell & Norman 2000 (chapter 15).
# Use this to invert diffuse transmittance, and calculate LAI

# z - zenith angle (radians)

# Extinction coefficient for direct beam; ellipsoidal LAD.
Kell <- function(z, x=1){
  sqrt(x^2 + tan(z)^2) /
    (x + 1.774*(x + 1.182)^-0.733)
}

# Transmittance for direct beam
Tdir <- function(z,LAI,clump=1,...){
  exp(-Kell(z,...)*clump*LAI)
}

# Transmittance for diffuse light:
# hemispherical integration of transmittance for direct light.
Tdiff <- function(LAI,...){
  
  fz <- function(z,LAI,...)Tdir(z,LAI,...)*sin(z)*cos(z)
  
  int <- integrate(fz, 0, pi/2, LAI=LAI, ...)$value
  
return(2 * int)
}

# Find LAI given some value of Tdiff.
LAI_Tdiff <- function(Td, ...){
  fl <- function(LAI, Td, ...)Tdiff(LAI=LAI,...) - Td
 
  r <- uniroot(fl, interval=c(0,20), Td=Td, ...)

return(r$root)
}






