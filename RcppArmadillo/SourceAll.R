setwd("~/econ722/RcppArmadillo") #location of .cpp files
library("Rcpp")
library("RcppArmadillo")

# Compile and call a minimal C++ function from R
sourceCpp("timesTwo.cpp")
timesTwo(3)

#R function to Generate a VAR(1) with uncorrelated errors
rVAR1 <- function(B, n) {
  p <- nrow(B)
  y <- matrix(0, p, n)
  for (i in 2:n) {
    errors <- rnorm(p)
    y[,i] = B %*% y[,(i - 1)] + errors
  }
  return(y)
}

M <- matrix(c(0.3, 0.1, 0.1, 0.3), 2, 2)
set.seed(1234)
rData <- rVAR1(M, 100)
sourceCpp("cppVAR1.cpp")
set.seed(1234)
cppData <- cppVAR1(M, 100)
all.equal(rData, cppData)

library(microbenchmark)
microbenchmark(rVAR1(M, 100), cppVAR1(M, 100))


# Example with multiple functions that call each other
sourceCpp("MultipleFunctions.cpp")
# notice that only the function h is available from R!
X <- matrix(rnorm(500), 100, 5)
foo <- h(X)
bar <- sum(diag(t(X) %*% X + t(X) %*% X))
foo - bar
