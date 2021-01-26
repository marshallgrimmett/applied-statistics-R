#####  Lab 09 R-code
#Load the file that contains CDI data
#CDI <- read.table("~/Documents/classes/m465/data/APPENC02.txt", quote="\"", comment.char="")

#remove the columns we will not use:
CDI<-APPENC02[,-c(1,2,3)]
#give names to the columns:
names(CDI)<-c('landarea','pop','pyoung','pold','nmed','nbed','crime','phs','pbach','poverty','unemp','income','personalinc','region')

# create a columns for normalized-pop squared
CDI$popsq<-((CDI$pop-mean(CDI$pop))/sd(CDI$pop))^2

# let's create the formula for regression without having to type all 15 variables:
form.full<-paste(names(CDI[! names(CDI) %in% c('crime','region')]),collapse ="+")   #crime is in position 7, remove it
form.full <- paste("crime~factor(region)+", form.full)
full.mod<-lm(as.formula(form.full),data=CDI)
# MSE of full model is needed to compute Cp
fullMSE<-anova(full.mod)["Residuals","Mean Sq"] 

# Best subsets:  one problem: region has 4 categories, so we need to handle it differently
# if we want to use the function I created to compare models
# I need to create dummy variables by hand
R2<-as.numeric(CDI$region==2)
R3<-as.numeric(CDI$region==3)
R4<-as.numeric(CDI$region==4)
# add dummy variables to data frame:
x<-cbind(CDI[! names(CDI) %in% c('crime', 'region')],R2,R3,R4)   # REMOVE county, state, region, and Y=crime; add dummy vars for region
Xnames<-c(names(CDI[ ! names(CDI) %in% c('crime','region')]), "R2","R3","R4") #ADD DUMMY VARS names FOR REGIONS
#regression formula to use with model best (best subsets)
formbest<-paste(Xnames,collapse ="+")
formbest <- paste("crime~", formbest)
# best subsets (one of the two lines below should work)
library(leaps)
# best<-regsubsets(formbest,data=data,nbest=3,nvmax=10)
best<-regsubsets(x=x,y=CDI$crime,nbest=3,nvmax=10)
# best<-regsubsets(crime~landarea+pop+pyoung+pold+nmed+nbed+phs+pbach+poverty+unemp+income+personalinc+popsq+R2+R3+R4,
#                  data=CDI,nbest=3,nvmax=10)

#now you can select the models by looking at the plots:
plot(best,scale="Cp")      #scale=c("bic", "Cp", "adjr2", "r2")
#or you can use a function I created matrix.selection()
# or instead of running regsubsets, you can run leaps

source("C:\\Users\\marsh\\Documents\\school\\amat565\\lab9\\modelselfunctions.R")
#NOTE: you need to "source" the file: modelselfunction.R 
#Once you saved it in your computer you can open it in the top-left window and click the icon "source"
#source('modelselfunctions.R')
attach(CDI)
matrix.selection(best,Xnames,'crime',fullMSE)
#if you want to know what are the variables, print Xnames. x1 is the first variable in Xnames, etc ...
print(Xnames)

#let's try leaps:
bestleaps<-leaps( x=x,y=crime,names=Xnames ,method="Cp",nbest=3)
cbind("p+1"=bestleaps$size,bestleaps$which,Cp=round(bestleaps$Cp,2))


mod1<-lm(crime~landarea+pop+pyoung+pold+nmed+nbed+phs+pbach+poverty+unemp+income+personalinc+popsq+R2+R3+R4)
summary(mod1)
mod2<-update(mod1,.~.-R2)
anova(mod2,mod1)
summary(mod2)
mod2<-update(mod2,.~.-pold)
summary(mod2)
mod2<-update(mod2,.~.-unemp)
summary(mod2)
mod2<-update(mod2,.~.-pyoung)
summary(mod2)
mod2<-update(mod2,.~.-phs)
summary(mod2)
mod2<-update(mod2,.~.-pbach)
summary(mod2)
mod2<-update(mod2,.~.-nbed)
summary(mod2)
mod2<-update(mod2,.~.-R4)
summary(mod2)
mod2<-update(mod2,.~.-R3)
summary(mod2)
mod2<-update(mod2,.~.-nmed)
summary(mod2)



modA<-lm(crime~landarea+pop+nmed+poverty+income+personalinc,data=CDI)
modB<-lm(crime~landarea+pop+poverty+income+personalinc+popsq,data=CDI)

# install.packages('caret')
require(caret)
CDIdata<-cbind(CDI[,-14],R2,R3,R4)
s1<-s2<-numeric(10)
flds <- createFolds(CDI$crime, k = 10, list = TRUE, returnTrain = FALSE)

for(i in 1:10){
  mod1<-lm(eval(modA$call[[2]]),data=CDIdata[-flds[[i]], ])
  mod2<-lm(eval(modB$call[[2]]),data=CDIdata[-flds[[i]], ])
  #test on removed fold
  pred1<-predict(mod1,newdata=CDIdata[flds[[i]],-7])
  pred2<-predict(mod2,newdata=CDIdata[flds[[i]],-7])
  s1[i]<-(1/length(flds[[i]]))*sum((CDI$crime[flds[[i]]]-pred1)^2)                # enter formula for the mean sum of square errors on the validation "fold" for mod1
  s2[i]<-(1/length(flds[[i]]))*sum((CDI$crime[flds[[i]]]-pred2)^2)             # enter formula for the mean sum of square errors on the validation "fold" for mod2
}

mean(s1)
mean(s2)

summary(modA)
summary(modB)
