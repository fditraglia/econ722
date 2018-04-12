#--------- Conditional CDF of y given U = u and A or A-complement
# This function is vectorized over y, b, and u
conditionalCDF <- function(y, b, u, rho, crit, A) {

  if(length(rho) > 1) stop('Invalid Input: rho must be a scalar.')
  if(length(crit) > 1) stop('Invalid Input: crit must be a scalar.')
  if(length(A) >1)  stop('Invalid Input: A must be a scalar.')
  if(rho <= 0) stop('Invalid Input: rho must be positive.')
  if(crit <= 0) stop('Invalid Input: c must be positive.')
  if(!is.logical(A)) stop('Invalid Input: A must be TRUE or FALSE')

  L <- (-crit - u) / rho
  R <- (crit - u) / rho
  Phi_y <- pnorm(y - b)
  Phi_R <- pnorm(R - b)
  Phi_L <- pnorm(L - b)

  if(A) {
    denom <- 1 - Phi_R + Phi_L
    num1 <- (y < L) * Phi_y
    num2 <- ((y >= L) & (y <= R)) * Phi_L
    num3 <- (y > R) * (Phi_y - Phi_R + Phi_L)
  } else {
    denom <- Phi_R - Phi_L
    num1 <- (y < L) * 0
    num2 <- ((y >= L) & (y <= R)) * (Phi_y - Phi_L)
    num3 <- (y > R) * (Phi_R - Phi_L)
  }
  return((num1 + num2 + num3) / denom)
}

#--------- Conditional quantiles function of y given U = u and A or A-complement
# This function is vectorized over y, b, and u
conditionalQuantile <- function(p, b, u, rho, crit, A) {

  if(length(rho) > 1) stop('Invalid Input: rho must be a scalar.')
  if(length(crit) > 1) stop('Invalid Input: crit must be a scalar.')
  if(length(A) >1)  stop('Invalid Input: A must be a scalar.')
  if(rho <= 0) stop('Invalid Input: rho must be positive.')
  if(crit <= 0) stop('Invalid Input: c must be positive.')
  if(!is.logical(A)) stop('Invalid Input: A must be TRUE or FALSE')

  L <- (-crit - u) / rho
  R <- (crit - u) / rho
  Phi_R <- pnorm(R - b)
  Phi_L <- pnorm(L - b)

  if(A) {
    denom <- 1 - Phi_R + Phi_L
    pstar <- Phi_L / denom
    argument <- p * denom + (p > pstar) * (Phi_R - Phi_L)
  } else {
    denom <- Phi_R - Phi_L
    argument <- p * denom + Phi_L
  }
  return(b + qnorm(argument))
}

#--------- Plot conditional CDF of y given U = u and A or A-complement
# This function is *not* vectorized over any of its arguments
plotCDF <- function(b, u, rho, crit, A) {
  if(length(b) > 1) stop('Invalid Input: rho must be a scalar.')
  eps <- 0.0001
  ylower <- conditionalQuantile(p = eps, b, u, rho, crit, A)
  yupper <- conditionalQuantile(p = 1 - eps, b, u, rho, crit, A)
  yseq <- seq(ylower, yupper, length.out = 501)
  Fseq <- conditionalCDF(yseq, b, u, rho, crit, A)
  plot(yseq, Fseq, type = 'l', xlab = 'y', ylab = 'CDF')
  abline(v = (-crit - u) / rho, lty = 2, col = 'red')
  abline(v = (crit - u) / rho, lty = 2, col = 'red')
}

#--------- Plot conditional Quantiles of y given U = u and A or A-complement
# This function is *not* vectorized over any of its arguments
plotQuantile <- function(b, u, rho, crit, A) {
  if(length(b) > 1) stop('Invalid Input: rho must be a scalar.')
  eps <- 0.0001
  pseq <- seq(eps, 1 - eps, length.out = 501)
  Qseq <- conditionalQuantile(pseq, b, u, rho, crit, A)
  plot(pseq, Qseq, type = 'l', xlab = 'p', ylab = 'Quantile')
  abline(h = (-crit - u) / rho, lty = 2, col = 'red')
  abline(h = (crit - u) / rho, lty = 2, col = 'red')
}

plotRejection <- function(alpha = 0.05, u, rho, crit = 1.96, A = TRUE) {
  bseq <- seq(-40, 40, length.out = 501)
  c_lower <- conditionalQuantile(alpha / 2, b = bseq, u, rho, crit, A)
  c_upper <- conditionalQuantile(1 - alpha / 2, b = bseq, u, rho, crit, A)
  matplot(bseq, cbind(c_lower, c_upper), type = 'l', lty = 1, col = 'black',
          xlab = expression(beta[0]), ylab = 'Critical Values')
}

#--------- Draw from the conditional distribution of (Yb, Yd) given A or A^c
drawConditional(n, b, d, rho, crit, A) {
  uniformSims <- runif(n)
  pA <- pnorm(crit - d) - pnorm(-crit - d)
  
  if(A) {
    pstar <- pnorm(-crit - d) / (1 - pA)
    Yd_conditional <- d + qnorm(uniformSims * (1 - pA) + (uniformSims > pstar) * pA)
  } else {
    
  }
  normalSims <- rnorm(n)
  Yb_conditional <- (1 - rho^2) * normalSims + (b + rho * (Yd_conditional - d))
}

# Some examples
plotCDF(b = 0, u = -0.1, rho = 0.9, crit = 2, A = TRUE)
plotCDF(b = 0, u = -0.1, rho = 0.9, crit = 2, A = FALSE)

plotQuantile(b = 0, u = -0.1, rho = 0.9, crit = 2, A = TRUE)
plotQuantile(b = 0, u = -0.1, rho = 0.9, crit = 2, A = FALSE)




# Simulation study
b <- 0
d <- 1
r <- 0.4
crit <- qnorm(1 - 0.05 / 2)

#nsims <- 5000
#Yd_A <- Yb_A <- rep(NA_real_, nsims)
#drawcount <- 0
#itercount <- 0
#maxiter <- 100 * nsims
#
#while((drawcount < nsims) & (itercount < maxiter)) {
#  y <- MASS::mvrnorm(1, c(b, d), Sigma = matrix(c(1, r, r, 1), 2, 2, byrow = TRUE))
#  Yb <- y[1]
#  Yd <- y[2]
#  if(abs(Yd) > crit){
#    drawcount <- drawcount + 1
#    Yb_A[drawcount] <- Yb
#    Yd_A[drawcount] <- Yd
#  }
#  itercount <- itercount + 1
#}

n <- 1000000

set.seed(1234)
y <- MASS::mvrnorm(n, c(b, d), Sigma = matrix(c(1, r, r, 1), 2, 2, byrow = TRUE))
Yb <- y[,1]
Yd <- y[,2]

yA <- y[abs(Yd) > crit,]
yA <- yA[sample(1:nrow(yA), 10000),]
Yb_A <- yA[,1]
Yd_A <- yA[,2]

hist(Yb_A)
U <- Yd_A - r * Yb_A

alpha <- 0.2

onesided <- function(u) {
  upper <- (crit - u) / r
  lower <- (-crit - u) / r
  denom <- 1 - pnorm(upper) + pnorm(lower)
  q <- seq(-10, 10, 0.005)
  num <- (q >= upper) * (pnorm(q) - pnorm(upper)) + pnorm(pmin(q, lower))
  prob <- num / denom
  q[which.min(abs(prob - alpha))]
  #plot(q, num / denom, type = 'l')
}

Vonesided <- Vectorize(onesided)
critical_values_approx <- Vonesided(U)

onesided(0.2)
conditionalQuantile(alpha, b, u = 0.2, rho = r, crit, A = TRUE)
critical_values <- conditionalQuantile(alpha, b, U, rho = r, crit, A = TRUE)

head(cbind(critical_values, critical_values_approx))

reject <- Yb_A < critical_values
mean(reject)
mean(Yb_A < qnorm(0.1))

my_Y <- mean(Yb_A)
my_U <- mean(U)

bseq <- seq(-20, 10, 0.01)
c_lower <- conditionalQuantile(0.025, b = bseq, u = my_U, rho = r, crit, A = TRUE)
c_upper <- conditionalQuantile(0.975, b = bseq, u = my_U, rho = r, crit, A = TRUE)
plot(bseq, c_lower, type = 'l')
points(bseq, c_upper, type = 'l')
abline(h = my_Y, lty = 2, col = 'red')

f <- function(b) {
  alpha <- 0.05
  Yobs <- 0.59
  Uobs <- 2.22
  conditionalQuantile(1 - alpha / 2, b, Uobs, r, crit, A = TRUE) - Yobs
}

uniroot(f, c(-20, 20))



