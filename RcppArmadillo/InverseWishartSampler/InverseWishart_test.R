#This script tests the three inverse Wishart samplers and shows that they product the same output. The C++ version is much faster though!
library("Rcpp")
library("RcppArmadillo")
library("MCMCpack")
source("InverseWishartMCMCpack.R")
sourceCpp("InverseWishart.cpp")

V <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
m <- c(-1, 1)
N <- 1000


set.seed(1234)
x <- mvrnorm(N, m, V)
x1 <- x[,1]
x2 <- x[,2]
plot(x1, x2)

# Calculate MLEs
V_hat <- cov(x) * (N - 1) / N
m_hat <- colMeans(x)

#Compare MCMCpack to my R version 
set.seed(4321)
foo <- myriwish(N - 1, N * V_hat)
set.seed(4321)
bar <- riwish(N - 1, N * V_hat)
all.equal(foo, bar)

#Compare MCMCpack to my C++ version
set.seed(4321)
baz <- rinvwish(1, N - 1, N * V_hat)  
all.equal(bar, baz[,,1])


# MCMCpack takes >10 times longer per draw
library(microbenchmark)
microbenchmark(rinvwish(1, N - 1, N * V_hat), riwish(N - 1, N * V_hat))

# If you want to draw repeatedly, MCMCpack > 150 times slower!
system.time(rinvwish(10000, N - 1, N * V_hat))
system.time(replicate(10000, riwish(N - 1, N * V_hat)))

# Check that the posterior is centered roughly at the MLE
set.seed(4321)
N_sims <- 10000
V_sims <- rinvwish(N_sims, N - 1, N * V_hat)
par(mfrow = c(2,2))
hist(V_sims[1,1,]); abline(v = V_hat[1,1], col = 'red', lwd = 2)
hist(V_sims[1,2,]); abline(v = V_hat[1,2], col = 'red', lwd = 2)
hist(V_sims[2,1,]); abline(v = V_hat[2,1], col = 'red', lwd = 2)
hist(V_sims[2,2,]); abline(v = V_hat[2,2], col = 'red', lwd = 2)
par(mfrow = c(1,1))