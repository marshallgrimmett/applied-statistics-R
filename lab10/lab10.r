breastdata <- read.csv("C:\\Users\\marsh\\Documents\\school\\amat565\\lab10\\breast.csv", header = TRUE)




breastdata$deg.malig <- as.numeric(as.character(breastdata$deg.malig))

sum(breastdata=='?')     # this will tell you how many missing data there are
breastdata[breastdata=='?'] <- NA      # change the '?' to NA
newbreast <-na.omit(breastdata)  # removes all rows with missing data NA

newbreast.model <- glm(Y ~ tsize + deg.malig, data=newbreast, family=binomial)
summary(newbreast.model)


anova(newbreast.model)



library(car)
diffdev<-summary(newbreast.model)$null.deviance -summary(newbreast.model)$deviance
print(paste('Difference in deviances: ',diffdev))

diffdf<-summary(newbreast.model)$df.null -summary(newbreast.model)$df.residual
print(paste('Difference in degrees of freedom: ',diffdf))
print(paste('p-value: ',1-pchisq(diffdev,diffdf)))

plot(newbreast.model)

attach(newbreast)
# lets move the values on the x-axis a little to better see what's going on
x<-jitter(tsize)
plot(Y~x,col=c("blue","green","red")[deg.malig],xlab="tsize",main="Recurrence vs Deg.Malig")
#add fitted values (colored by deg.malig)
points(tsize,newbreast.model$fitted,col=c("blue","green","red")[deg.malig],pch=22)
#add fitted values curve
lines(lowess(tsize,newbreast.model$fitted),col="black",lwd=2)
#look at a threshold of .5
abline(h=.5,col='grey')

predict(newbreast.model,data.frame(tsize=(28),deg.malig=3),type="response")
