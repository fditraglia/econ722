library(shiny)


shinyServer(function(input, output) {
  
  n <- reactive({ input$n })
  
  
  
  output$M1probPlot <- renderPlot({
    
    mu <- seq(from = -0.5, to = 0.5, by = 0.001)
    pAIC <- pnorm(-sqrt(2) - sqrt(n()) * mu) + (1 - pnorm(sqrt(2) - sqrt(n()) * mu))  
    pBIC <- pnorm(-sqrt(log(n())) - sqrt(n()) * mu) + (1 - pnorm(sqrt(log(n())) - sqrt(n()) * mu)) 
    y.max <- max(max(pBIC), max(pAIC))
    y.min <- min(min(pBIC), min(pAIC))
    plot(mu, pAIC, ylim = c(y.min, y.max), xlab = expression(mu), ylab = 'Probability of Selecting M1', type = 'l', col = 'red', lwd = 2)
    points(mu, pBIC, type = 'l', col = 'blue', lty = 2, lwd = 2)
    legend('bottomright', legend = c('AIC', 'BIC'), lty = c(1,2), col = c('red', 'blue'), lwd = c(2,2))
    
  })
  
  
})
