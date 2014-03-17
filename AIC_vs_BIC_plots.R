#This script replicates Figures 4.1 and 4.2 of Claeskens & Hjort (2008) - Model Selection and Model Averaging

#Figure 4.1 gives the probability of selecting Model 1 for the AIC and BIC when n = 1000
mu <- seq(from = -0.5, to = 0.5, by = 0.001)

n <-  1000

pAIC <- pnorm(-sqrt(2) - sqrt(n) * mu) + (1 - pnorm(sqrt(2) - sqrt(n) * mu))  

pBIC <- pnorm(-sqrt(log(n)) - sqrt(n) * mu) + (1 - pnorm(sqrt(log(n)) - sqrt(n) * mu)) 

y.max <- max(max(pBIC), max(pAIC))
y.min <- min(min(pBIC), min(pAIC))
plot(mu, pAIC, ylim = c(y.min, y.max), xlab = expression(mu), ylab = 'Probability of Selecting M1', type = 'l', col = 'red', lwd = 2)
points(mu, pBIC, type = 'l', col = 'blue', lty = 2, lwd = 2)
legend('bottomright', legend = c('AIC', 'BIC'), lty = c(1,2), col = c('red', 'blue'), lwd = c(2,2))