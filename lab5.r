boxplot(cuckoo$Length~as.factor(cuckoo$Species))

cuckoo.model<-lm(Species~Length,data=cuckoo)
cuckoo.model2<-lm(Length~Species,data=cuckoo)

summary(cuckoo.model)

anova(cuckoo.model)
anova(cuckoo.model2)

plot(Before~Loss, data=weightloss, col=Diet+1)

plot(Height~Age, data=diet, col=Diet+1)
