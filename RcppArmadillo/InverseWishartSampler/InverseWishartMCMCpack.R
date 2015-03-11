# These functions produce the same output as rwish and riwish in the MCMCpack package
myrwish <- function (v, S) {
  p <- nrow(S)
  L <- t(chol(S))
  A <- matrix(0, p, p)
  diag(A) <- sqrt(rchisq(p, v:(v - p + 1)))
  if (p > 1) {
    for(row in 2:p){
      for(col in 1:(row-1)){
        A[row, col] <- rnorm(1)
      }
    }
  }
  return(tcrossprod(L %*% A))
}

myriwish <- function(v, S){
  p <- nrow(S)
  S.inv <- solve(S)
  L <- t(chol(S.inv))
  A <- matrix(0, p, p)
  diag(A) <- sqrt(rchisq(p, v:(v - p + 1)))
  if (p > 1) {
    for(row in 2:p){
      for(col in 1:(row-1)){
        A[row, col] <- rnorm(1)
      }
    }
  }
  return(crossprod(solve(L %*% A)))
}