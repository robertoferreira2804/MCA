library(pracma)
library(expm)
library(writexl)
library(spatstat.geom)

tic()
clear()
options(digits = 6)
# Define the parameters
d <- 60000 # Number of intervals
M=2*d  
n=5
#In-Control
gamma0=3.2 #scale
delta0=4.8 #shape
#out-of-control
gamma1=3.2 #scale
delta1=4.8 #shape
#Ly
LI=0
L=Lfinal # Use the Ly program
GRID <- L/M
vetor <- seq(LI, L, by = 2*GRID)
tv=size(vetor)
tv=tv[2]
vetorf<-rbind(vetor[1:(tv-1)],vetor[2:tv])
vetorf<-t(vetorf)
vetorfm<-(vetorf[,1]+vetorf[,2])/2
vetorf<-cbind(vetorf,vetorfm)
B <- matrix(0, nrow = M/2, ncol = M/2)

for (J in 1:(M/2)) {
  for (K in 1:(M/2)) {
    LJ <- vetorf[J,3]
    LK <- vetorf[K,3]
    if (LK-LJ>=0){
      RES1 <- pweibull((LK - LJ) + GRID, delta0, gamma0)
      RES3 <- pweibull((LK - LJ) - GRID, delta0, gamma0)
      B[J, K] <- (RES1 - RES3)
    }
  }
}

#B[M/2,M/2]=1
SB=rowSums(B)
B=B/SB

VI=c(rep(0,M/2))
VI[1]=1

VF <- VI
for (i in 1:n) {
  VF <- VF %*% B
}

LL=weighted.quantile(vetorf[,3], VF, probs=c(pnorm(-3),1-pnorm(-3)))

Al=vetorf[,3][which.min(abs(vetorf[,3] - LL[1]))]
Bl=vetorf[,3][which.min(abs(vetorf[,3] - LL[2]))]
LL[1]=Al
LL[2]=Bl
TesteA=(vetorf[,3]<LL[1]|vetorf[,3]>LL[2])
ARL0=1/(sum(TesteA*VF))

cat('LCL Xbar =',LL[1]/n,"\n")
cat('LCL Y =',LL[1],"\n")
cat('UCL Xbar =',LL[2]/n,"\n")
cat('UCL Y =',LL[2],"\n")
cat('ARL0 =',ARL0,"\n")
cat('Largura =',L/(M/2),"\n")
toc()
tic()
##################out-of-control#############
clear("B")
tic()
GRID <- L/M
vetor <- seq(LI, L, by = 2*GRID)
tv=size(vetor)
tv=tv[2]
vetorfa<-rbind(vetor[1:(tv-1)],vetor[2:tv])
vetorfa<-t(vetorfa)
vetorfma<-(vetorfa[,1]+vetorfa[,2])/2
vetorfa<-cbind(vetorfa,vetorfma)
B <- matrix(0, nrow = M/2, ncol = M/2)

for (J in 1:(M/2)) {
  for (K in 1:(M/2)) {
    LJ <- vetorfa[J,3]
    LK <- vetorfa[K,3]
    if (LK-LJ>=0){
      RES1 <- pweibull((LK - LJ) + GRID, delta1, gamma1)
      RES3 <- pweibull((LK - LJ) - GRID, delta1, gamma1)
      B[J, K] <- (RES1 - RES3)
    }
  }
}
SB=rowSums(B)
B=B/SB
VFa <- VI
for (i in 1:n) {
  VFa <- VFa %*% B
}

TesteB=(vetorfa[,3]<LL[1]|vetorfa[,3]>LL[2])
ARL1=1/(sum(TesteB*VFa))
cat('ARL1 =',ARL1,"\n")
toc()
  
  
  
  
  
  
  
