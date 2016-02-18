#Correction for Unreliability

#Standard correction for unreliability

#rxy = observed correlation
#rxx = reliability of X variable
#ryy = reliability of Y variable
#If either reliablity coefficient is ommitted, the function will not correct for unreliability in that variable.
corunrel <- function(rxy, rxx=1.0, ryy=1.0) {
  if (rxx > 1.0 || ryy > 1.0) {
    warning("Reliabilities cannot exceed 1.0", call. = F)
  }
  corrected <- rxy/sqrt(rxx*ryy)
  if (corrected > 1.0) {
    warning("The Corrected correlation exceeds 1.0; this is set to 1.00")
    return (1.00)
  } else {
    return (corrected)
  }
}

corunrel(.30, .80, .85) #0.3638034
corunrel(.30) #.30; no corrections for unreliability were made.
corunrel(.30, .67) #0.3665083
corunrel(.30, ryy=.67) #0.3665083; identical to prior example

corunrel(.60, .40, .30) #Returns 1.00 with a warning; correlations can't exceed 1.0
corunrel(.30, 1.67) #Returns .232147 with a warning that reliabilities can't exceed 1.0
