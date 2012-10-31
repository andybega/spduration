## predict.spdur

for weibull:
  
  X.train<-as.matrix(X)
Z.train<-as.matrix(Z)
y.train<-as.matrix(y)

X<-as.matrix(X.test)
Z<-as.matrix(Z.test)
y<-as.matrix(y.test)

reps <- sims 

cyhat.train <- matrix(NA, nrow(X.train), reps) 
cyhat <- matrix(NA, nrow(X), reps)

for (i in 1:reps){
  draw<-mvrnorm(1,coeff,vcv)
  
  lxbeta <- X.train %*% draw[1:ncol(X.train)]
  pxbeta <- Z.train %*% draw[(ncol(X.train)+1):(ncol(X.train)+ncol(Z.train))]
  alph <- 1*draw[ncol(X.train)+ncol(Z.train)+1]
  
  alph <-exp(-alph)
  lyhat <- exp(-lxbeta)
  pyhat <- 1/(1+exp(-pxbeta))
  cyhat.train[,i]<-pyhat/((lyhat*y.train[,2])^alph+pyhat*(1-(lyhat*y.train[,2])^alph))
  
  lxbeta <- X %*% draw[1:ncol(X)]
  pxbeta <- Z %*% draw[(ncol(X)+1):(ncol(X)+ncol(Z))]
  alph <- 1*draw[ncol(X)+ncol(Z)+1]
  
  alph<-exp(-alph)
  lyhat <- exp(-lxbeta)
  pyhat <- 1/(1+exp(-pxbeta))
  cyhat[,i]<-pyhat/((lyhat*y[,2])^alph+pyhat*(1-(lyhat*y[,2])^alph))
}

predvalues.train <- apply(cyhat.train, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)         
predvalues <- apply(cyhat, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)