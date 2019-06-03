#1
seed<-1
n<-1000

x<-runif(1000)
y<-sqrt(100/(1-x)) - 10 #F^-1(x)
print(y[1:5])

#2

#first, find needed size N using small n

n<-1000
#create matrix that will hold the waiting times
#each column is a form of transportation
x<-matrix(numeric(n*3),n)

#put random normal numbers as waiting times for each column
#convert 5 mins 30 seconds to 5.5 mins
#convert 10 mins 20 seconds to 10.333 mins
for(idx in 1:3){
  x[,idx]<-(4.833333)*runif(n) + 5.5
}

#make vector of all the lowest waiting times
lowest<-apply(x,1,min)

#find the mean of the waiting times
est_wait<-mean(lowest)


#now, find n to make error no larger than .1 minute
N<-ceiling(((2.58*1.1*sd(lowest))/.01)^2)
print(N)

#now, re-do above calculations with N to find avg wait time with .01 error tolerance

N<-77489

#create matrix that will hold the waiting times
#each column is a form of transportation
x<-matrix(numeric(N*3),N)

#put random normal numbers as waiting times for each column
#convert 5 mins 30 seconds to 5.5 mins
#convert 10 mins 20 seconds to 10.333 mins
for(idx in 1:3){
  x[,idx]<-(4.833333)*runif(N) + 5.5
}

#make vector of all the lowest waiting times
lowest<-apply(x,1,min)

#find the mean of the waiting times
est_wait<-mean(lowest)

#find estimation error
est_err<-(2.58*sd(lowest))/sqrt(N)


#3

#define constants
S_0<-100 #initial stock price
K<-130 #strike price
MT<-1 #maturity time
r<-0.03 #risk-free rate
sigma<-0.5 #volatility
d<-12   #monitoring frequency
delta<-MT/d
n<-10000

#generate Brownian Motion first

x<-matrix(rnorm(n*d),nrow=n) #normal numbers for BM
BM<-sqrt(delta)*t(apply(x,1,cumsum)) 

#generate geometric BM next
t_grid<-seq(delta,MT,length.out = d)
s_paths<-100*exp(sweep(0.05*BM,MARGIN = 2,-0.095*t_grid,'+'))

#generate payoffs next
arith_mean_part<-apply(s_paths,1,sum)/12

payoffs<-pmax(arith_mean_part-100,0)*exp(-r)


#mean price = mean(payoffs)
  
meanPrice<-mean(payoffs)
print(mean_price)




#4

n<-10

#first, let's get one Z value.

x<-runif(n)
is_greater<-x>0.5
is_smaller<-x<0.5

greater<-sum(is_greater)
smaller<-sum(is_smaller)
Z<-greater-smaller



#now, let's use MC to get 10^4 Z values

n<-10^4

unif_matrix<-matrix(numeric(n*n),nrow=n)

for(idx in 1:nrow(unif_matrix)){
  unif_matrix[idx,]<-runif(n)
}
is_greater<-unif_matrix>0.5
is_smaller<-unif_matrix<0.5

greater<-apply(is_greater,1,sum)
smaller<-apply(is_smaller,1,sum)

Z<-greater-smaller

#now, find all Z values > 5

biased<-abs(Z)>5

#to get probability you'd think it's biased as Z values >5 / total number of Z values
prob_biased<-sum(biased)/n
print(prob_biased)
















