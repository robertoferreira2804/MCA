library (pracma)
clear ()
tic()
n=5 #Sample size
target_alpha<-0.002699796 #Desired significance level
lower_target_alpha<-target_alpha/2
upper_target_alpha<-1-target_alpha/2
corridas=10000000
#In-Control
k<-5.4 #shape
lambda=2.2 #scale
#out-of-control
k1=5.4 #shape
lambda1=2.2 #scale lambda1=1.9
set.seed(12345)

R<-(matrix(rweibull(n*corridas,k,lambda),n,corridas)) 
Rsum=colSums(R)
LCL=quantile(Rsum,lower_target_alpha)
UCL=quantile(Rsum,upper_target_alpha)
ARL0=1/(mean(Rsum<LCL)+mean(Rsum>UCL))

R1<-(matrix(rweibull(n*corridas,k1,lambda1),n,corridas)) 
Rsum1=colSums(R1)
ARL1=1/(mean(Rsum1<LCL)+mean(Rsum1>UCL))

cat('LCLy=', LCL, "\n")
cat('UCLy=', UCL, "\n")
cat('ARL0=', ARL0, "\n")
cat('ARL1=', ARL1, "\n")
toc()



