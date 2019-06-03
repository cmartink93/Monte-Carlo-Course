#homework 5, problem 12

# try small n to see how big n needs to be so that error = .01
n <- 10000
err <- 0.01

T1 <- 10*runif(n) + 70 #initial temperature
Xj <- rnorm(n) #random normal numbers

T2 <- 14 + 0.8*T1 + 3*rnorm(n)
T3 <- 14 + 0.8*T2 + 3*rnorm(n)
T4 <- 14 + 0.8*T3 + 3*rnorm(n)
T5 <- 14 + 0.8*T4 + 3*rnorm(n)
T6 <- 14 + 0.8*T5 + 3*rnorm(n)
T7 <- 14 + 0.8*T6 + 3*rnorm(n)

oneWeek <- cbind(T1,T2,T3,T4,T5,T6,T7)

#determine the required size  given the error tolerance
hat_sigma<-sd(oneWeek) #calculate the sample standard deviation
N<-ceiling((2.58*1.1*hat_sigma/err)^2)
print(N)

##############################################################

T1 <- 10*runif(N) + 70 #initial temperature
Xj <- rnorm(N) #random normal numbers

T2 <- 14 + 0.8*T1 + 3*Xj[1]
T3 <- 14 + 0.8*T2 + 3*Xj[2]
T4 <- 14 + 0.8*T3 + 3*Xj[3]
T5 <- 14 + 0.8*T4 + 3*Xj[4]
T6 <- 14 + 0.8*T5 + 3*Xj[5]
T7 <- 14 + 0.8*T6 + 3*Xj[6]


oneWeek <- cbind(T1,T2,T3,T4,T5,T6,T7) #puts all 7 days in one matrix with each day as a 
                                        #column and each weekly trial as a row


findLows<-function(weekMatrix){
  isLow <- matrix(0, nrow = nrow(weekMatrix), ncol = ncol(weekMatrix))
  for(row in 1:nrow(weekMatrix)) {nu
    for(col in 1:ncol(weekMatrix)) {
      isLow[row,col] <- ifelse(oneWeek[row,col] >= 70.0, 0, 1); isLow
    }
  }
  return(isLow)
}

findNumLows<-function(isLow){
  numLowsVector<-ifelse(rowSums(isLow) >= 2, 100, 0)
  print(numLowsVector)
  # for(row in 1:nrow(isLow)) {
  #   numLows <- c(numLows,ifelse(rowSums(oneWeek[row,]) >= 2, 100, 0))
  #   }
  numLowsInteger <- length(numLowsVector[ numLowsVector == 100 ])
  return(numLowsInteger)
}
 

weekLows <- findLows(oneWeek)
numLows <- findNumLows(weekLows)
#weekLows <- apply(oneWeek,1, function(x) #sets any maximum temps below 70 to 1 and 
 # {ifelse(oneWeek >= 70.0, 0, 1)})        #all other values to 0

#numLows <- apply(weekLows,1, function(x) length(weekLows[ weekLows >= 1 ])) #finds the number of weeks with 
                                                                            # 2 or more temps below 70


fairPrice <- 100*numLows/N #finds the average price
fairPrice

 
  
error <-2.58*sd(oneWeek)/sqrt(N)
