#Corrections for Unreliability

#######################################
#Standard correction for unreliability#
#######################################

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
    warning("The Corrected correlation exceeds 1.0; this is set to 1.00", call. = F)
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

###################################################################################
#Correcting a correlation for unreliability, but allowing error terms to correlate#
###################################################################################

#rxy = observed correlation
#ree = correlation between error terms for the X and Y variables
#rxx = reliability of X variable
#ryy = reliability of Y variable
#Note: Default assumes no correlation for the error term; this would simplify it to the standard correction.
corunrel.err <- function(rxy, ree=0.0, rxx=1.0, ryy=1.0) {
  if (rxx > 1.0 || ryy > 1.0) {
    warning("Reliabilities cannot exceed 1.0", call. = F)
  }
  corrected <- (rxy - ree*sqrt(1-rxx)*sqrt(1-ryy))/sqrt(rxx*ryy)
  if (corrected > 1.0) {
    warning("The Corrected correlation exceeds 1.0; this is set to 1.00", call. = F)
    return (1.00)
  } else {
    return (corrected)
  }
}

corunrel.err(.30, .20, .85, .75) #0.3272274
corunrel.err(.30, rxx=.80, ryy=.85) #0.3638034; without error specified, this is the same as corunrel
corunrel.err(.30, .41) #.30; no changes are made without reliabilities specified.
