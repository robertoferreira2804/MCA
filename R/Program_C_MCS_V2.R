rm(list = ls())
t0 <- Sys.time()

n <- 5 #Sample size
alpha <- 2 * (1 - pnorm(3)) # Desired significance level = 0.002699796
lower_alpha <- alpha / 2
upper_alpha <- 1 - alpha / 2
runs <- 10000000

#In-Control
lambda_0  <- 2.2 #scale
k_0 <- 5.4 #shape

#out-of-control
k_1 <- 5.4      #shape. Use NULL if you do not want H1 calculation.
lambda_1 <- 1.9 #scale to calculate ARL_1. Alternative: using NULL with sh below.
sh <- 0.05         # defined only if lambda_1 = NULL, for Table 5

######## Table 5 with shift on E[X] ###########
if (is.null(lambda_1)) {
  ex_H0 <- lambda_0 * gamma(1 + 1 / k_0)
  ex_H1 <- ex_H0 * (1 + sh)
  lambda_1 <- ex_H1 / gamma(1 + 1 / k_1)  
} else{
  sh <- 0
}
################################################

base_seed <- 1
set.seed(base_seed + 1000L + n)  # seed per n (CL simulation)

R <- (matrix(rweibull(n*runs,k_0,lambda_0),n,runs)) #to calculate control limits
Rsum  <- colSums(R)
LCLy <- quantile(Rsum, lower_alpha)
UCLy  <- quantile(Rsum, upper_alpha)

set.seed(base_seed + 2000L + n)
R0 <- (matrix(rweibull(n*runs,k_0,lambda_0),n,runs)) #another database to calculate ARL_0
Rsum0  <-  colSums(R0)
ARL_0  <-  1 / (mean(Rsum0 < LCLy) + mean(Rsum0 > UCLy))

set.seed(base_seed + 3000L + n + as.integer(10 * (k_1 * 10 + (sh + 0.5) * 100)))
R1 <- (matrix(rweibull(n*runs,k_1,lambda_1),n,runs)) #to calculate ARL_1
Rsum1  <-  colSums(R1)
ARL_1  <-  1 / (mean(Rsum1 < LCLy) + mean(Rsum1 > UCLy))

cat('LCLy = ', LCLy, "\n")
cat('UCLy = ', UCLy, "\n")
cat('ARL0 = ', ARL_0, "\n")
cat('ARL1 = ', ARL_1, "\n")
cat("Time:", as.double(Sys.time()) - as.double(t0), "s\n")
beepr::beep(5)


