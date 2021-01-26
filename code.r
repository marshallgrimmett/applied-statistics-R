plot(Conc~Thick,data=pelican_eggs)

abline(eggs.model)



abrasion <- read.csv("C:/Users/marsh/Documents/school/amat565/abrasion.csv", header=FALSE)
View(abrasion)



source('pairs.r')

pairs(abrasion, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)



abrasion.model <- lm(Loss ~ x1 + x2, data=abrasion)



summary(abrasion.model)


new.abrasion <- data.frame(
  x1 = 71,
  x2 = 201
)

predict(abrasion.model, newdata = new.abrasion, interval = "confidence", )

anova(abrasion.model)