library(shiny)

d.AIC <- function(n.T){return(2)}
d.BIC <- function(n.T){return(log(n.T))}
d.Hodge <- function(n.T){return(sqrt(n.T))}

risk <- function(n.T, mu, d.T){
  a <- -sqrt(d.T(n.T)) - sqrt(n.T) * mu
  b <- sqrt(d.T(n.T)) - sqrt(n.T) * mu
  first.term <- b * dnorm(b) - a * dnorm(a) 
  second.term <- (n.T * mu^2 - 1) * (pnorm(b) - pnorm(a))
  return(1 + first.term + second.term)
}


shinyServer(function(input, output) {
  
  n <- reactive({ input$n })
  
  
  
  output$RiskPlot <- renderPlot({
    
    mu <- seq(from = -2, to = 2, by = 0.01)
    risk.mu <- cbind(risk(n(), mu, d.AIC), 
                     risk(n(), mu, d.BIC))
    
    matplot(mu, risk.mu, ylab = 'Risk', xlab = expression(mu), type = 'l', lwd = 2, col = c("red", "blue"))
    legend('bottomright', legend = c('AIC', 'BIC'), lty = c(1,2), col = c('red', 'blue'), lwd = c(2,2))
    
    
    
  })
  
  
})
