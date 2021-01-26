y<-yahooprices.weekly$Open[60:10]    # ordered in increasing date
n<-length(y)
t<-1:n

yahoo.model <- lm(y~t+I(t^2), data=yahooprices.weekly)
summary(yahoo.model)

plot(y~t, data=yahooprices.weekly)
lines(t,yahoo.model$fitted.values,type="l",col="green")

plot(yahoo.model$res[2:n]~yahoo.model$res[1:n-1])

library(lmtest)
dwtest(y~t+I(t^2))

acf(yahoo.model$residuals)

n<-length(yahoo.model$residuals)
RES<-yahoo.model$residuals[2:n]
lagRES<-yahoo.model$residuals[1:n-1]
rho<-lm(RES~0+lagRES)$coef
rho
