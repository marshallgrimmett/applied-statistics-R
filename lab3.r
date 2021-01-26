# eggs.model<-lm(Conc~Thick,data=pelican_eggs)
# 
# p
# 
# plot(Conc~Thick,data=pelican_eggs)
# 
# abline(eggs.model)

# abrasion <- read.csv("C:/Users/marsh/Documents/school/amat565/abrasion.csv", header=FALSE)
# View(abrasion)

source('pairs.r')

pairs(abrasion, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)

abrasion.model <- lm(Loss ~ x1 + x2, data=abrasion)

summary(abrasion.model)

885.1611 - 6.5708*(71) - 1.3743*(201)

new.abrasion <- data.frame(
  x1 = 71,
  x2 = 201
)

predict(abrasion.model, newdata = new.abrasion, interval = "confidence", )

anova(abrasion.model)




X <- matrix(c(1, 45, 162, 1, 55, 233, 1, 61, 232, 1, 66, 231, 1, 71, 231, 1, 71, 237, 1, 81, 224), byrow=TRUE, nrow=7)
XXinv <- solve(t(X) %*% X)






