# eggs.model<-lm(Conc~Thick,data=pelican_eggs)
# 
# p
# 
# plot(Conc~Thick,data=pelican_eggs)
# 
# abline(eggs.model)

# abrasion <- read.csv("C:/Users/marsh/Documents/school/amat565/abrasion.csv", header=FALSE)
# View(abrasion)

# source('pairs.r')
# 
# pairs(abrasion, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)
# 
# abrasion.model <- lm(Loss ~ x1 + x2, data=abrasion)
# 
# summary(abrasion.model)
# 
# 885.1611 - 6.5708*(71) - 1.3743*(201)
# 
# new.abrasion <- data.frame(
#   x1 = 71,
#   x2 = 201
# )
# 
# predict(abrasion.model, newdata = new.abrasion, interval = "confidence", )
# 
# anova(abrasion.model)
# 
# 
# 
# 
# X <- matrix(c(1, 45, 162, 1, 55, 233, 1, 61, 232, 1, 66, 231, 1, 71, 231, 1, 71, 237, 1, 81, 224), byrow=TRUE, nrow=7)
# XXinv <- solve(t(X) %*% X)
# 



# anova(water.model)
# 
# 
# 113.544
# 11
# 
# 136.021
# 14
# 
# ((136.021-113.544)/(14-11))/(113.544/11)
# 
# 
# water.model2 <- lm(USAGE~PROD+PAYR, data=water)
# anova(water.model, water.model2)
# 
# 
# plot(water.model2)
# 
# 
# install.packages("lmtest")
# library(lmtest)
# bptest(water.model2)
# 
# 
# shapiro.test(water.model2$residuals)
# 
# shapiro.test(residuals(water.model2))





panel.hist<-function(x, ...) {
  usr<-par("usr");
  on.exit(par(usr));
  par(usr=c(usr[1:2], 0, 1.5));
  h<-hist(x, plot = FALSE);
  breaks <-h$breaks;
  nB <-length(breaks);
  y<-h$counts; y <-y/max(y);
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <-par("usr");
  on.exit(par(usr));
  par(usr = c(0, 1, 0, 1));
  r <-abs(cor(x, y,use="complete.obs"));
  txt <-format(c(r, 0.123456789), digits = digits)[1];
  txt <-paste0(prefix, txt);
  if(missing(cex.cor))
    cex.cor <- 0.8/strwidth(txt);
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(woodstrength, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)

woodstrength.model <- lm(Strength ~ Conc + I(Conc^2), data=woodstrength)

summary(woodstrength.model)

plot(Strength~Conc, data=woodstrength)

lines(woodstrength$Conc,woodstrength.model$fitted.values,type="l",col="green")

abline(lm(woodstrength$Strength~woodstrength$Conc),col='red')

plot(woodstrength.model)

install.packages("lmtest")
library(lmtest)
bptest(woodstrength.model)

shapiro.test(woodstrength.model$residuals)

# confint(woodstrength.model, level=0.95)
new.woodstrength <- data.frame(
  Conc = 6
)
predict(woodstrength.model, newdata = new.woodstrength, interval = "confidence", )




# plotting setup
par(mfrow = c(3,2), mar = c(2,2,1,1)) # set up 6 subplots

#set up  of a vector to keep track of s (square-root of MSE) and the adjR2 for different polynomial models

#initialize dataframe for RMSE
RMSE <- data.frame('kth.oder' = NA, 's' = NA,'adjR2'=NA) # empty data frame to store RMSE and adjR^2
# list values for COnd in order - for plotting
Conc.val<-list(Conc=seq(0,20,by=.05))

#run the loop to plot several models and compute s and adjR2

k<-1:15  # we are computing 15 modles but plotting only 6 of them

for (i in 1:length(k)){
  # build polynomial models of degree k with orthogonal polynomials:
  model <- lm(Strength~poly(Conc,k[i]),data=woodstrength)
  
  # calculate RSME and store it for further usage
  RMSE[i,1] <- k[i] # store k-th order
  RMSE[i,2] <- summary(model)$sigma     # calculate s
  RMSE[i,3] <- summary(model)$adj      # calculate adjR2
  # predict only for 6 models that we are going to plot
  if(i<4 || i==5 || i==10 || i==15 ){
    predictions <- predict(model, newdata = Conc.val)
    # plot
    plot(Strength~Conc, pch=16, col='blue',data=woodstrength)
    lines(Conc.val$Conc, predictions, lwd=2, col='red')
    text(x=8,y=10, paste0('k=',k[i], ', s=', round(RMSE[i,2],3),' adjR2=',round(RMSE[i,3],2))) # annotate the plot
  }
}

for (i in 1:length(k)){
  # build models
  model <- lm(Strength~poly(Conc,k[i]),data=woodstrength)
  
  # calculate RSME and store it for further usage
  RMSE[i,1] <- k[i] # store k-th order
  RMSE[i,2] <- summary(model)$sigma     # calculate s
  RMSE[i,3] <- summary(model)$adj        # calculate adjR2
  # predict
  if(i<4 || i==5 || i==10 || i==15 ){
    predictions <- predict(model, newdata = Conc.val)
    # plot
    plot(Strength~Conc, pch=16, col='blue',data=woodstrength, xlim=c(-5,20))
    lines(Conc.val$Conc, predictions, lwd=2, col='red')
    text(x=8,y=10, paste0('k=',k[i], ', s=', round(RMSE[i,2],3),' adjR2=',round(RMSE[i,3],2))) # annotate the plot
  }
}




# plotting setup
par(mfrow = c(2,1))
plot(RMSE[,1], RMSE[,2],
     xlab = 'k-th order',
     ylab = 's',
     type = 'b',
     col = 'blue',
     pch = 16)
plot(RMSE[,1], RMSE[,3],
     xlab = 'k-th order',
     ylab = 'adjR2',
     type = 'b',
     col = 'blue',
     pch = 16)




#Define split vector 80 - 20
n<-length(woodstrength$Conc)
set.seed(25) # for reproducibility
split <- sample(1:n,.8*n)   # this is an index set
head(split)  # you can see it is an index set

# split data set
train.set <- woodstrength[split,]
val.set <- woodstrength[-split,]

# check dimensions of the training and validation set
dim(train.set)
dim(val.set)








# setup
RMSE <- data.frame('kth.oder' = NA, 'RMSE.train' = NA,'RMSE.val' = NA) # empty data frame to store RMSE
vals <- list('Conc' <- seq(0,16, by = 0.05)) # set up vector used for prediction

# run  loop
k <- seq(1,10) #k-th order

for (i in k){
  # build models
  model <- lm(Strength ~ poly(Conc,k[i]), data = train.set)
  
  # calculate RMSE and store it for further usage
  RMSE[i,1] <- k[i] # store k-th order
  RMSE[i,2] <- sqrt(sum((fitted(model)-train.set$Strength)^2)/(length(train.set$Strength)-2)) # calculate RMSE of the training set # Note: this should be    summary(model)$sigma   but because the dataset was too small, I had to modify it a bit not to get zeros or negative numbers when we do the RMSE with the  small validation set
  # predict
  predictions <- predict(model, newdata = val.set)
  RMSE[i,3] <- sqrt(sum((predictions-val.set$Strength)^2)/(length(val.set$Strength)-2)) # calculate RMSE of the validation set
  # denominator should be length-k[i]-1 but validation set is too small
}

# plot RMSE for training and validation set
par(mfrow = c(1,1), mar = c(5,5,2,2)) # set up 1 subplots

plot(RMSE[,1], RMSE[,2],
     xlab = 'k-th order',
     ylab = 'RMSE',
     ylim = c(min(RMSE[,c(2,3)]), max(RMSE[,c(2,3)])),
     type = 'b',
     col = 'blue',
     pch = 16)
lines(RMSE[,3],
      type = 'b',
      col = 'red',
      pch = 16)
legend('topright',
       legend = c('training set','validation set'),
       lty=c(1,1),
       col = c('blue','red'))









best.order <- RMSE[which.min(RMSE[,3]),1]
best.order
final.model <- lm(Strength ~ poly(Conc, best.order), data = train.set)

# predictions
predictions <- predict(final.model, newdata = vals, interval="confidence", level = 0.95)

# plot data
plot(Strength~Conc, pch = 16, ylab = '', xlab= '',data=woodstrength)

# plot fit and confidence levels
lines(vals[[1]], predictions[,'fit'], lwd = 2, col = 'blue')
lines(vals[[1]], predictions[,'upr'], lwd = 2, lty = 2, col = 'blue')
lines(vals[[1]], predictions[,'lwr'], lwd = 2, lty = 2, col = 'blue')

legend('topright',
       legend = c('observed data','prediction','signal'),
       lty = c(NA, 1, 1),
       pch = c(16, NA, NA),
       cex = 0.75,
       col = c(1, 'blue', 'green'),
       lwd = c(NA, 2, 2))