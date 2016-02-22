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
}
