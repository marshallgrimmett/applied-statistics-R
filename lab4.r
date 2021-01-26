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

pairs(water, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)

water.model <- lm(USAGE~TEMP+PROD+DAYS+PAYR+HOUR, data=water)

summary(water.model)

anova(water.model)


113.544
11

136.021
14

((136.021-113.544)/(14-11))/(113.544/11)


water.model2 <- lm(USAGE~PROD+PAYR, data=water)
anova(water.model, water.model2)


plot(water.model2)


install.packages("lmtest")
library(lmtest)
bptest(water.model2)


shapiro.test(water.model2$residuals)

shapiro.test(residuals(water.model2))




