reg.conf.intervals <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  model <- lm(y ~ x) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- model$coefficients[1]
  b1 <- model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- min(x):max(x)
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  semean <- summary(model)$sigma * sqrt(1 / n + (x_new - mean(x))^2 / sum((x - mean(x))^2))
  #  sqrt(sum((y - y.fit)^2) / (n - 2)) 
  semeanp <- summary(model)$sigma * sqrt(1+1 / n + (x_new - mean(x))^2 / sum((x - mean(x))^2))
  
  # Warnings of mismatched lengths are suppressed
  mean.upper <- suppressWarnings(y.fit + t.val * semean)
  mean.lower <- suppressWarnings(y.fit - t.val * semean)
  pred.upper <- suppressWarnings(y.fit + t.val * semeanp)
  pred.lower <- suppressWarnings(y.fit - t.val * semeanp)
  
  # Plot the fitted linear regression line and the computed confidence bands
  plot(x, y, cex = 1.5, pch = 21, bg = 'gray', main='Confidence Intervals for Regression of PCB and Thickness of Egg Shell', xlab='Thickness', ylab='PCB')
  abline(model,col='red')
  lines(x_new,mean.upper, col = 'blue', lty = 2, lwd = 2)
  lines(x_new,mean.lower, col = 'blue', lty = 2, lwd = 2)
  lines(x_new,pred.upper, col = 'green', lty = 3, lwd = 2)
  lines(x_new,pred.lower, col = 'green', lty = 3, lwd = 2)
}