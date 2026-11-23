# Program C: Monte Carlo simulation for Weibull control limits and ARL.
# Default example: Section III, Table II (n = 5, 10 million runs).
#
# Other Monte Carlo studies in the paper:
# - Section III, Figure 1: 500 replications of 10 million runs.
# - Section IV-C: 500 replications of 10 million runs for ARL0 and ARL1.
# - Section V, Table VIII: 500 replications of 10 million runs for each
#   n in {5, 30, 100}, with lambda_0 = 4 and k_0 = 5.
# - Section VI, Table IX: 50 million runs per sample size and shift
#   scenario (n in {5, 10, 15, 30, 50}; lambda_0 = 4, k_0 = 5).
# - Section VII: Monte Carlo validation of the scalability results,
#   with 500 replications of 10 million runs. Table X reports MCA runtimes.
#
# For multiple replications, set B = 500 and use the corresponding
# scenario inputs. This mode exports replicate results and means.
# The histogram and confidence intervals are not generated here.

rm(list = ls())
t0 <- Sys.time()

n <- 5 # Sample size
alpha <- 2 * (1 - pnorm(3)) # Desired significance level = 0.002699796
lower_alpha <- alpha / 2
upper_alpha <- 1 - alpha / 2
runs <- 10000000 # Runs per replication (50000000 for Table IX).

B <- 1 # Use 1 for Tables II and IX, or 500 for the repeated studies above.

# In-Control
lambda_0  <- 2.2 #scale
k_0 <- 5.4 #shape

#out-of-control
k_1 <- 5.4      # Out-of-control shape.
lambda_1 <- 1.9  #scale to calculate ARL_1. Alternative: using NULL with sh below.
sh <- 0.05         # Relative mean shift when lambda_1 is NULL (Table IX).

# Derive the scale from the relative mean shift when lambda_1 is NULL.
if (is.null(lambda_1)) {
  ex_H0 <- lambda_0 * gamma(1 + 1 / k_0)
  ex_H1 <- ex_H0 * (1 + sh)
  lambda_1 <- ex_H1 / gamma(1 + 1 / k_1)
} else {
  sh <- 0
}
################################################

if (B == 1) {
  
  # ============================================================
  # Single replication (Table II or a scenario from Table IX)
  # ============================================================
  
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
  
  # Scenario-specific seed for the out-of-control simulation.
  set.seed(base_seed + 3000L + n + as.integer(10 * (k_1 * 10 + (sh + 0.5) * 100)))
  R1 <- (matrix(rweibull(n*runs,k_1,lambda_1),n,runs)) #to calculate ARL_1
  Rsum1  <-  colSums(R1)
  ARL_1  <-  1 / (mean(Rsum1 < LCLy) + mean(Rsum1 > UCLy))
  
  cat('LCLy = ', LCLy, "\n")
  cat('UCLy = ', UCLy, "\n")
  cat('ARL0 = ', ARL_0, "\n")
  cat('ARL1 = ', ARL_1, "\n")
  
} else {
  
  # ============================================================
  # Multiple replications (Figure 1, Section IV-C, Table VIII, Section VII)
  # ============================================================
  
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Please install it using install.packages('writexl').")
  }
  
  Resultados <- matrix(0, B, 4)
  
  for (ii in 1:B) {
    
    print(ii)
    
    R <- (matrix(rweibull(n*runs,k_0,lambda_0),n,runs)) #to calculate control limits
    Rsum  <- colSums(R)
    LCLy <- quantile(Rsum, lower_alpha)
    UCLy  <- quantile(Rsum, upper_alpha)
    
    R0 <- (matrix(rweibull(n*runs,k_0,lambda_0),n,runs)) #another database to calculate ARL_0
    Rsum0  <-  colSums(R0)
    ARL_0  <-  1 / (mean(Rsum0 < LCLy) + mean(Rsum0 > UCLy))
    
    R1 <- (matrix(rweibull(n*runs,k_1,lambda_1),n,runs)) #to calculate ARL_1
    Rsum1  <-  colSums(R1)
    ARL_1  <-  1 / (mean(Rsum1 < LCLy) + mean(Rsum1 > UCLy))
    
    Resultados[ii,1] <- ARL_0
    Resultados[ii,2] <- ARL_1
    Resultados[ii,3] <- LCLy
    Resultados[ii,4] <- UCLy
  }
  
  Resultados <- data.frame(Resultados)
  names(Resultados) <- c("ARL_0", "ARL_1", "LCLy", "UCLy")
  
  writexl::write_xlsx(Resultados, "Resultados.xlsx")
  
  cat("Mean ARL_0 = ", mean(Resultados$ARL_0), "\n")
  cat("Mean ARL_1 = ", mean(Resultados$ARL_1), "\n")
  cat("Mean LCLy = ", mean(Resultados$LCLy), "\n")
  cat("Mean UCLy = ", mean(Resultados$UCLy), "\n")
}

cat("Time:", as.double(Sys.time()) - as.double(t0), "s\n")

