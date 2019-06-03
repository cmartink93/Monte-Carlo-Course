#hw4, problems 6 & 7

#problem 6
n<-10000
xUnif<-runif(n)
inputNums=c(0:9)
poissonCDF<-function(numbers){
  #creates & returns the set of poisson CDF numbers given lambda = 1 and a set of input numbers
  totalCDF <- vector('numeric')
  idx=1
  cdf=(exp(-1) / factorial(idx))
  totalCDF <- c(cdf)
  while(idx<=length(numbers)){
    cdf= cdf + (exp(-1) / factorial(idx))
    idx = idx + 1
    totalCDF <- c(totalCDF,cdf)
  }
  return(totalCDF)
}
poisCDFSet <- poissonCDF(inputNums)

unifToPoisson <- function(unifNum,poisCDF){
  #transforms given set of random uniform numbers into poisson numbers of lambda = 1
  poisNum <- vector('numeric')
  for(num in unifNum){
    if (num <= poisCDF[1]){poisNum <- c(poisNum,1)}
    else if (num <= poisCDF[2]){poisNum <- c(poisNum,2)}
    else if (num <= poisCDF[3]){poisNum <- c(poisNum,3)}
    else if (num <= poisCDF[4]){poisNum <- c(poisNum,4)}
    else if (num <= poisCDF[5]){poisNum <- c(poisNum,5)}
    else if (num <= poisCDF[6]){poisNum <- c(poisNum,6)}
    else if (num <= poisCDF[7]){poisNum <- c(poisNum,7)}
    else if (num <= poisCDF[8]){poisNum <- c(poisNum,8)}
    else if (num <= poisCDF[9]){poisNum <- c(poisNum,9)}
    else if (num <= poisCDF[10]){poisNum <- c(poisNum,10)}
    else {poisNum <- c(poisNum,11)}
  }
  
  return(poisNum)
}

unifToPoisson(xUnif,poisCDFSet)

#problem 7

n<-1000
X<-runif(n)
Y<- -1 * log(X^-1 - 1)