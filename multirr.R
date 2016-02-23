#Multivariate Range Restriction Correction

#res = restricted covariance matrix, including criteria
#unres = unrestricted covariance matrix of predictors only
multirr <- function(res, unres) {
  if (nrow(res) != ncol(res)) {
    stop("Restricted matrix is not square.")
  }
  if (nrow(unres) != ncol(unres)) {
    stop("Unrestricted matrix is not square.")
  }
  npred <- nrow(unres) #Computes number of predictors
  ncrit <- nrow(res) - npred #computes number of criteria
  
  #Creating the matrices for the actual math
  respredmat <- res[1:npred,1:npred] #matrix of preditor intercorrelations
  resxy <- res[npred+1:nrow(res), 1:npred] #matrix of correlations between predictors and criteria
  rescrit <- res[nrow(res)-ncrit:nrow(res),nrow(res)-ncrit:nrow(res)] #matrix of correlations among criteria
  
  #Math for the correction
  unresxy <- resxy %*% solve(respredmat) %*% res
  unrescrit <- rescrit - resxy %*% (solve(respredmat) - (solve(respredmat)%*%unres%*%solve(respredmat))) %*% t(resxy)
  
  #Reassembling the matrix
  left <- rbind(unres,unresxy)
  right <- rbind(t(unresxy),unrescrit)
  return(cbind(left,right))
}

#Note:
#This should work, but it untested at this moment.
