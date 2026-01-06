library(pracma)
#library(expm)
#library(writexl)
library(spatstat.geom)

rm(list = ls())
tic()

options(digits = 6)
# Define the parameters
d <- 55000 # Number of states
#M <- 2 * d  
n <- 5

#In-Control
lambda_0 <- 4.0 #scale
k_0 <- 5.0 #shape

#Ly
S0 <- 0
Ly  <-  29.5554920052575    # (using Program_D_Ly.R)
Ly  <-  30   # only for the article section 4.2 (didactic example), d = 10  
Sd <- Ly # Use the Program_D_Ly.R for other cases

delta <- Ly/d
S_i <- seq(S0, Sd, by = delta)
State <- rbind(S_i[1:d],S_i[2:(d+1)])
State <- t(State)
E_m <- (State[,1] + State[,2])/2
State <- cbind(State, E_m)
M <- matrix(0, nrow = d, ncol = d)

for (i in 1:(d)) {
  for (j in i:(d)) {
    Ei_m <- State[i,3]
    Ej_m <- State[j,3]
    if (Ej_m - Ei_m>=0){
      M[i, j] <- pweibull((Ej_m - Ei_m) + 0.5 * delta, k_0, lambda_0) -
                 pweibull((Ej_m - Ei_m) - 0.5 * delta, k_0, lambda_0)
    }
  }
}

Sum_M <- rowSums(M)
M <- M/Sum_M

b <- c(1, rep(0,d-1))

fY <- b
for (i in 1:n) {
  fY <- fY %*% M
}
fY[fY < 0] <- 0  # due to potential rounding issues
fY <- fY / sum(fY)   # sum = 1

CL <- weighted.quantile(State[,3], fY, probs=c(pnorm(-3),1-pnorm(-3)))

LCLy <- State[,3][which.min(abs(State[,3] - CL[1]))]
UCLy <- State[,3][which.min(abs(State[,3] - CL[2]))]

aux1 <- (State[,3] < LCLy|State[,3]>UCLy)
ARL0 <- 1 / (sum(aux1 * fY))

cat('d =', d, "\n")
cat('LCLx =', LCLy/n, "\n")
cat('UCLx =', UCLy/n, "\n")
cat('LCLy =', LCLy, "\n")
cat('UCLy =', UCLy, "\n")
cat('ARL0 =', ARL0, "\n")
cat('Delta =', Ly/(d), "\n")
toc()


##################out-of-control#############
tic()
clear("M")

#out-of-control
lambda_1 <- 3.2 #scale
k_1 <- 4.8 #shape

#Ly
S0 <- 0
Ly  <-  24.0357569982349   # for lambda_1 = 3.2 and k_1 = 4.8
Sd <- Ly # Use the Program_D_Ly.R for other cases

delta <- Ly/d
S_i <- seq(S0, Sd, by = delta)
State <- rbind(S_i[1:d],S_i[2:(d+1)])
State <- t(State)
E_m <- (State[,1] + State[,2])/2
State <- cbind(State, E_m)
M <- matrix(0, nrow = d, ncol = d)

for (i in 1:(d)) {
  for (j in i:(d)) {
    Ei_m <- State[i,3]
    Ej_m <- State[j,3]
    if (Ej_m - Ei_m>=0){
      M[i, j] <- pweibull((Ej_m - Ei_m) + 0.5 * delta, k_1, lambda_1) -
        pweibull((Ej_m - Ei_m) - 0.5 * delta, k_1, lambda_1)
    }
  }
}

Sum_M <- rowSums(M)
M <- M/Sum_M

b <- c(1, rep(0,d-1))

fY <- b
for (i in 1:n) {
  fY <- fY %*% M
}
fY[fY < 0] <- 0  # due to potential rounding issues
fY <- fY / sum(fY)   # sum = 1

aux2 <- (State[,3] < LCLy|State[,3] > UCLy)
ARL1 <- 1 / (sum(aux2 * fY))
cat('ARL1 =', ARL1, "\n")
toc()
  
  
  
  
  
  

  
