#9


n<-10^3 #number of target numbers we want to generate
efficiency<-2 #efficiency of the method
N<-ceiling(n/efficiency*1.1)
Y<-runif(N) #uniform random numbers as candidate numbers
W<-runif(N) #uniform random numbers used in criterion 
X<-numeric(n)

X<-Y[which(efficiency*(2*Y)-efficiency*2*(Y^2)>=W)]
X<-X[1:n]



#10

#VARIABLES

S_0<-100 #initial stock price
MT<-(12/52) #maturity time
r<-0 #risk-free rate
sigma<-0.4 #volatility
d<-12   #monitoring frequency
delta<-MT/d


#GENERATE SAMPLE PRICE PATHS

#generate nxd pseudo-random normal numbers (n sample paths with d time nodes for each path)
x<-matrix(rnorm(n*d),nrow=n)

#generate n sample paths of Brownian Motion
BM<-sqrt(delta)*t(apply(x,1,cumsum))

#generate n sample paths of stock price
grid<-seq(delta,MT,length.out=d) #time grid
S_paths<-S_0*exp(sweep(sigma*BM,MARGIN=2,(r-sigma^2/2)*grid,'+'))


#APPLY SAMPLE PRICE PATHS TO LOOKBACK OPTION

LBCallPayoff<-(S_paths[,d]-apply(S_paths,1,min))*exp(-r*MT)
LBPutPayoff<-(apply(S_paths,1,max)-S_paths[,d])*exp(-r*MT)
LBCallPrice<-mean(LBCallPayoff)
LBPutPrice<-mean(LBPutPayoff)

LBCallPrice
LBPutPrice

