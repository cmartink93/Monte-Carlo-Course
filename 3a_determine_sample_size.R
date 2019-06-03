#generate sample of lifetimes

nLife<-1000 #initial sample size of lifetimes
errRefund<-0.5 #error tolerance of refund mean
xLife<-rexp(nLife,rate=1/30) #generate 1000 random samples of lifetimes from exp[0,inf] distribution


#using xLife, generate sample of refunds as a matrix and apply
#ifelse so that the only nonzero elements in the resulting matrix
#are the lifetimes small enough to give a refund (i.e. less than 5)
lifetimesMatrix<-matrix(sample(xLife,nLife*1,replace=TRUE),nrow=1)
findAllFails<-apply(lifetimesMatrix,1, function(x) 
          {ifelse(lifetimesMatrix >= 5, 0,100)})


#determine the required size of refund sample y given the error tolerance
hat_sigma<-sd(findAllFails) #calculate the sample standard deviation
N<-ceiling((2.58*1.1*hat_sigma/err)^2)

#do the estimation using another independent sample with N points
N = 10^6
xLifeN<-rexp(N,rate=1/30) #generate 1000 random samples of lifetimes from exp[0,inf] distribution
lifetimesMatrixN<-matrix(sample(xLifeN,N*1,replace=TRUE),nrow=1)
findAllFailsN<-apply(lifetimesMatrixN,1, function(x) 
          {ifelse(lifetimesMatrixN >= 5, 0,100)})
expRefund<-mean(findAllFailsN)
error<-2.58*sd(findAllFailsN)/sqrt(N)
expRefund
error

#95% Confidence Interval
err95CI<-1.645*sd(findAllFailsN)/sqrt(N)
expRefund - err95CI
expRefund + err95CI


