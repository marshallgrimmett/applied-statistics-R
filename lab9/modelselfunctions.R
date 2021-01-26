######
#  Functions: PRESS, Criteria, matrix.selection
######

PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  return(PRESS)
}

Criteria<-function(model,FullMSE,label=F){
  s.model<-summary(model)
  R2adj<-s.model$adj.r.squared     #adjusted r-sq
  sse<-anova(model)["Residuals","Sum Sq"]   # SSE
  dfsse<-anova(model)["Residuals","Df"]      # df of SSE
  samplesize<- 1+ sum(anova(model)$"Df")     # n
  nparameters<- as.integer(samplesize - dfsse)           # p+1
  Cp<-sse/FullMSE+2*nparameters-samplesize
  AIC<-samplesize*(log(sse)-log(samplesize))+2*nparameters
  press<-PRESS(model)
  if(label==T)
    c("p+1"=nparameters,R2adj=round(R2adj,4),Cp=round(Cp,2),AIC=round(AIC,2),PRESS=press)
  else
    c(nparameters,round(R2adj,4),round(Cp,2),round(AIC,2),press)
}




#######
#subset.model is the output from regsubsets()
#Yname is the name of the response variable in the model
#Xdata data.frame with predictor variables IMPORTANT: variables need to have names
# IMPORTANT: DOES NOT WORK with Categorical data with more than 2 categories!!!!

matrix.selection<-function(best.subset.model,Xnames,Yname,FullMSE){
  m<-dim(summary(best.subset.model)[[1]])
df.out<-data.frame()
for(i in 1:m[1])
{
  foo<-summary(best.subset.model)[[1]][i,-1]
  form <- Xnames[foo]
  #form <- best.subset.model$xnames[foo]  #right thing to do but don't know how to handle dummy vars
  form <- paste(form, collapse = " + ")
  form <- paste(Yname, form,sep='~')
  model <- lm(as.formula(form))
  df.out<-rbind(df.out,Criteria(model,FullMSE))
}
names(df.out)<-c('p+1','R2adj','Cp','AIC','PRESS')
dfsummary<- ifelse(summary(best.subset.model)[[1]],1,0)
nnames<-length(dimnames(dfsummary)[[2]])
U<-cbind(rep('x',(nnames-1)),seq(1:(nnames-1)))
dimnames(dfsummary)[[2]]<-c('',apply(U,1,function(x) paste(x,collapse="")) )
#'x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13')
row.names(dfsummary)<-NULL
df.out<-cbind(dfsummary[,-1],df.out)
df.out
}


##############################
matrix.leaps<-function(leaps.model,Xdata,Yname,FullMSE){
  m<-dim(leaps.model$which)[[1]]
  df.out<-data.frame()
  for(i in 1:m[1])
  {
    foo<-leaps.model$which[i,]
    form <- names(Xdata)[foo]              
    #form <- leaps.model$xnames[foo]
    form <- paste(form, collapse = " + ")
    form <- paste(Yname, form,sep='~')
    model <- lm(as.formula(form))
    df.out<-rbind(df.out,Criteria(model,FullMSE))
  }
  names(df.out)<-c('p+1','R2adj','Cp','AIC','PRESS')
  dfsummary<- ifelse(leaps.model$which,1,0)
  nnames<-length(dimnames(dfsummary)[[2]])
  U<-cbind(rep('x',(nnames)),seq(1:(nnames)))
  dimnames(dfsummary)[[2]]<-c(apply(U,1,function(x) paste(x,collapse="")) )
  #'x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11','x12','x13')
  row.names(dfsummary)<-NULL
  db.out<-cbind(dfsummary,df.out)
  db.out
}

############################   This part is not complete
# change Xdata to Xnames and pass varaible n
#  best model with Cp
#rsq
#rss
#adjr2
#Cp
#bic
#AIC

best.model<-function(leaps.model,Xdata,Yname,FullMSE,crit=Cp){
  slp<-summary(leaps.model)

if(crit=='Cp'){
x<-leaps.model$Cp<(leaps.model$size+1)
y<-leaps.model$Cp>(leaps.model$size-1)
idx<-x*y
#minCpdiff<-min(abs(lp3$Cp[idx==1]-lp3$size[idx==1]))
#ibest<-which(abs(lp3$Cp-lp3$size)==minCpdiff) pics model with all variables
ibest<-min(which(idx==1))   #choose least vars
}

if(crit=='BIC')
  {n <- length(Xdata[,1])
  p <- length(lp3$label)
  r<-dim(leaps.model$which)[1]
  R<-numeric(r)
  for(i in 1:r)
  {
    form<-names(Xdata)[leaps.model$which[i,]]
    form <- paste(form, collapse = " + ")
    form <- paste("Yname~", form)
    sse<- anova(lm(as.formula(form)))["Residuals","Sum Sq"] 
    R[i]<-n*log(sse)-n*log(n)+p  *lon(n)     #bic
  }
  ibest <- seq(along = slp$bic)[slp$bic == min(slp$bic)]
  }

if(crit=='AIC')
  {n <- length(Xdata[,1])
  p <- length(lp3$label)
  r<-dim(leaps.model$which)[1]
  R<-numeric(r)
  for(i in 1:r)
  {
    form<-names(Xdata)[leaps.model$which[i,]]
    form <- paste(form, collapse = " + ")
    form <- paste("Yname~", form)
    sse<- anova(lm(as.formula(form)))["Residuals","Sum Sq"] 
    R[i]<-n*log(sse)-n*log(n)+2*p       #aic
  }
  ibest <- seq(along = R)[R == min(R)]
  }

  if(crit=='adjrsq')
   { r<-dim(leaps.model$which)[1]
     R<-numeric(r)
     for(i in 1:r)
     {
      form<-names(Xdata)[leaps.model$which[i,]]
      form <- paste(form, collapse = " + ")
      form <- paste("Yname~", form)
      R[i]<-summary(lm(as.formula(form)))$adj.r.squared 
    }
   ibest <- seq(along = R)[R == max(R)]
  }
  if(crit=='rsq')
  { r<-dim(leaps.model$which)[1]
  R<-numeric(r)
  for(i in 1:r)
  {
    form<-names(Xdata)[leaps.model$which[i,]]
    form <- paste(form, collapse = " + ")
    form <- paste("Yname~", form)
    R[i]<-summary(lm(as.formula(form)))$r.squared 
  }
  ibest <- seq(along = R)[R == max(R)]
  }
  if(crit=='rss'|| crit=="see")
  { r<-dim(leaps.model$which)[1]
  R<-numeric(r)
  for(i in 1:r)
  {
    form<-names(Xdata)[leaps.model$which[i,]]
    form <- paste(form, collapse = " + ")
    form <- paste("Yname~", form)
    R[i]<-anova(lm(as.formula(form)))["Residuals","Sum Sq"] 
  }
  ibest <- seq(along = R)[R == min(R)]
  }

    
  
foo <- leaps.model$which[ibest, ]
form <- names(Xdata)[foo]
form <- paste(form, collapse = " + ")
form <- paste("Yname~", form)
out.best <- lm(as.formula(form))  
}
