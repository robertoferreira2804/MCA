library(pracma)

tic()
clear()
options(digits = 8)
# Define the parameters
d <- 10000 # Number of intervals
M=2*d  
n=5
#In-Control/ out-of-control
gamma0=4 #scale
delta0=5 #shape
LSC=23.672
LIC=12.503
#Ly
LI=0
L=30 # Use the Ly program
GRID <- L/M
vetor <- seq(LI, L, by = 2*GRID)
tv=size(vetor)
tv=tv[2]
vetorf<-rbind(vetor[1:(tv-1)],vetor[2:tv])
vetorf<-t(vetorf)
vetorfm<-(vetorf[,1]+vetorf[,2])/2
vetorf<-cbind(vetorf,vetorfm)
vetorf_rep <- do.call(rbind, replicate(n, vetorf, simplify = FALSE))
bloco <- rep(1:n, each = nrow(vetorf))
Estados <- data.frame(bloco, vetorf_rep[,1], vetorf_rep[,2],vetorf_rep[,3])
colnames(Estados) <- c("B", "IE","FE", "ME")
Estados=data.matrix(Estados)

Ii=zeros(1,n*d)
for (s in 1:d){
  p1=Estados[s,2]
  p2=Estados[s,3]
  Ii[s]=(pweibull(p2, delta0, gamma0)-pweibull(p1, delta0, gamma0))
}
S1=sum(Ii[,1:(n*d-1)])
Ii[1,n*d]=1-S1
Ii=c(0,Ii)
B <- matrix(0, nrow = n*(M/2), ncol = n*(M/2))

for (J in 1:(n*(M/2))) {
  for (K in 1:(n*(M/2))) {
    LJ <- Estados[J,4]
    LK <- Estados[K,4]
    rj<-Estados[J,1]
    rk<-Estados[K,1]
    if (LK-LJ>=0 & ((rk==(rj+1)) | (rk==n & rj==n))){
      RES1 <- pweibull((LK - LJ) + GRID, delta0, gamma0)
      RES3 <- pweibull((LK - LJ) - GRID, delta0, gamma0)
      B[J, K] <- (RES1 - RES3)
    }
  }
}

for (l1 in 1:size(B)[1]){
k1=max(which(B[l1,]  > 0))
k2=sum(B[l1,])
if (k2<1){
  B[l1,k1]=(1-k2)+B[l1,k1]
}
}
B_new <- cbind(0, B)

B_new <- rbind(Ii, B_new)

for (u in 1:(size(B_new)[1]-1)){
 u1=Estados[u,4]
 u2=Estados[u,1]
 if ((u1>LSC & u2==n) | (u1<LIC & u2==n)){
   B_new[(u+1),]=0
   B_new[(u+1),1]=1
 }
if (u1>=LIC & u1<=LSC & u2==n){
     B_new[(u+1),]=0
     B_new[(u+1),1]=1
}   
   
 }

Z=which((Estados[,4] < LIC & Estados[,1]==n)
        | (Estados[,4] > LSC & Estados[,1]==n))
Z=Z+1
R=B_new[-Z,-Z]
rm(B)
rm(B_new)
gc()
ref=size(R)[1]
u<-c(rep(0, ref)) 
u[1]=1
I=diag(rep(1,ref)) 
ones=ones(ref,1)
P1=solve((I-R))
ARL=u%*%P1%*%ones 

cat('ARL =',ARL,"\n")
cat('ARL_Final =',ARL/(n+1),"\n") 

toc()



