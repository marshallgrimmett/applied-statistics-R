names(data)<-c("Y","X")
data.model<-lm(Y~X,data=data)
plot(Y~X, data=data)
abline(data.model)

install.packages("car")
library(MASS)
library(car)
boxCox(data.model, plotit = TRUE)

# y.transformed <- yjPower(data$Y, -1.5)
data.model2<-lm(log(Y)~X,data=data)
plot(log(Y)~X, data=data)
abline(data.model2)
predict(data.model2, newdata=data.frame(X=6), interval="confidence")



head(longley)  # to see the variables
y<-longley[,1]  # response variables
n<-length(y)
X<- longley[,-c(1,6)]   # explanatory variables (with one removed)
source("C:/Users/marsh/Documents/school/amat565/pairs.r")
pairs(X, panel=panel.smooth, diag.panel=panel.hist, lower.panel=panel.cor)





#prepare cross validation
set.seed(100) # set seed to replicate results

# not working the same for all    trainingIndex <- sample(1:n, 0.8*n) # take a sample without replacement of indeces 1 to n
trainingIndex<-c(5 , 4 , 8,  1,  6 ,12,  9, 15, 16,  2, 10, 14)
train.X <- X[trainingIndex, ] # training data
train.y<-y[trainingIndex]
test.X <- X[-trainingIndex, ] # test data
test.y<-y[-trainingIndex]
p<-dim(X)[2] 

# this is ridge regression, to be used with train.X and train.y
ridge.lm<-function(l,X,y){ 
  p=5 #p<-dim(X)[2]  
  n<-length(y) 
  designX<-as.matrix(cbind(intercept=rep(1,n),X)) 
  XXinv<-solve(t(designX) %*% designX + l*diag(p+1)) 
  beta.l<-XXinv %*% t(designX) %*% y 
  fit<- designX %*% beta.l 
  res<- y-fit 
  vif.l<-diag(XXinv)[-1]*(n-1)* diag(var(designX))[-1] 
  r2<-1-sum(res^2)/sum((y-mean(y))^2) 
  s2<- sum(res^2)/(n-p-1)
  result<-list(beta.l=beta.l,fitted=fit,residuals=res,sigma=sqrt(s2),Rsqr=r2,vif.l=vif.l)
  return(result) 
}

mod.lm <- lm(GNP.deflator~.,data=longley[trainingIndex,-6]) #-6
betas.lm = coef(mod.lm)
vif(mod.lm)






lambdas<-c(0,.002,.004,.006,.008,.01,.02,.03,.04,.05,.1,.5,1,10)         
m<-length(lambdas)
betas.rdg<-matrix(0,nrow=p+1,ncol=m)
vif.rdg<-matrix(0,nrow=p,ncol=m)
for(j in 1:m){ 
  out<-ridge.lm(lambdas[j],train.X,train.y) 
  betas.rdg[,j]<-out$'beta.l' 
  vif.rdg[,j]<-out$'vif.l'
}
row.names(betas.rdg)<-c("intercept",names(X))
t(rbind(lambda=round(lambdas,3),round(betas.rdg,4)))
cor(longley)





row.names(vif.rdg)<-names(X)
round(t(rbind(lambda=lambdas,vif.rdg,R2=sapply(lambdas,FUN=function(s){ridge.lm(s,train.X,train.y)$Rsqr}))),3)




u=length(train.y)

#fitted values
fit.ridge<- as.matrix(cbind(rep(1,u),train.X)) %*% betas.rdg[,4]

#se ridge train
sqrt(mean((train.y-fit.ridge)^2))
u=length(test.y)
fit.ridge.test<- as.matrix(cbind(rep(1,u),test.X)) %*% betas.rdg[,4]

#se test
sqrt(mean((test.y-fit.ridge.test)^2))
cbind(actual=test.y,linear=predict(mod.lm,newdata = as.data.frame(test.X)),ridge=fit.ridge.test )
