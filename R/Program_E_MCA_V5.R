# ============================================================
# Beta Program (row-normalized like the original) - works for any d
# ============================================================
if (!requireNamespace("spatstat.geom", quietly = TRUE)) {
  stop("Package 'spatstat.geom' is required. Please install it via install.packages('spatstat.geom').")
}
library(spatstat.geom)
rm(list = ls())
t0 <- Sys.time()

options(digits = 6)

# -----------------------
# User parameters
# -----------------------
d <- 10          # number of states 
n <- 5               # sample size

#in-control
lambda_0 <- 4.0      # Weibull scale (in-control)
k_0 <- 5.0           # Weibull shape (in-control)

######## ATTENTION: Ly = 30 for the didactic example Table 2 and 3
Ly_0  <-  29.5555    # (using Program_D_Ly.R)
Ly_0 <- 30             # didactic example- Table 3; for general cases use Program_D_Ly.R
######################################################

#out-of-control
k_1 <- 5 #shape. Use NULL if you do not want H1 calculation.
lambda_1 <- 3.5 #scale to calculate ARL_1. Alternative: using NULL with sh below.
sh <- -0.05  # only if lambda_1 = NULL
Ly_1  <-  25.8611   # (using Program_D_Ly.R)

######## Table 5 with shift on E[X] ###########
if (is.null(lambda_1)) {
  ex_H0 <- lambda_0 * gamma(1 + 1 / k_0)
  ex_H1 <- ex_H0 * (1 + sh)
  lambda_1 <- ex_H1 / gamma(1 + 1 / k_1)  
}

################################################

####### IN - CONTROL ########
S0 <- 0
Sd <- Ly_0

# Choose truncation behavior for kernel k:
use_jmax <- (d > 1000)     # jmax use only for "usual" cases
#p_target <- 1 - 1e-11      # used only if you decide to use p-target stopping (optional)

# -----------------------
# Build states
# -----------------------
delta <- Ly_0 / d
S_i <- seq(S0, Sd, by = delta)

State <- rbind(S_i[1:d], S_i[2:(d + 1)])
State <- t(State)
E_m <- (State[, 1] + State[, 2]) / 2
State <- cbind(State, E_m)         # State[,3] is midpoint

# -----------------------
# Build kernel k = first row of the (unnormalized) Toeplitz-upper-triangular M
# k[h] corresponds to M[1, 1+h-1] for h=1..d (then optionally truncated to jmax)
# -----------------------
k <- numeric(d)
jmax <- d

for (j in 1:d) {
  dif_E <- State[j, 3] - State[1, 3]
  hi <- dif_E + 0.5 * delta
  lo <- dif_E - 0.5 * delta
  
  # Bin probability (same as your original formula)
  k[j] <- pweibull(hi, shape = k_0, scale = lambda_0) -
    pweibull(lo, shape = k_0, scale = lambda_0)
  
  # Guard against tiny negative values due to rounding/cancellation
  if (k[j] < 0) k[j] <- 0
  
  # Optional: stop by exact underflow-to-zero (not robust for very small d)
  if (use_jmax && k[j] == 0) { jmax <- j - 1; break }
}

if (jmax < 1) jmax <- 1
k <- k[1:jmax]
m <- length(k)

# -----------------------
# Precompute row-normalization denominators S_i (equivalent to rowSums(M) in original)
# For row i (1..d), only h=1..min(m, d-i+1) are available (columns cannot exceed d):
#   S_i = sum_{h=1..min(m, d-i+1)} k[h]
# -----------------------
cs <- cumsum(k)                         # cs[h] = sum_{r=1..h} k[r]
m_i <- pmin(d:1, m)                     # m_i[i] = min(m, d-i+1)
S_i <- cs[m_i]                          # S_i[i] = sum(k[1:m_i[i]])

# Safety: avoid division by zero (should not happen if k[1]>0)
S_i[S_i <= 0] <- 1

# -----------------------
# One-step update reproducing original row normalization:
#   g[j] = sum_{i=1..j} f[i] * ( k[j-i+1] / S_i[i] )
# Implemented as banded Toeplitz accumulation (O(d*m))
# -----------------------
step_band_rowNorm <- function(f, k, S_i) {
  d <- length(f)
  m <- length(k)
  g <- numeric(d)
  
  # h = j - i + 1  (kernel index)
  # for a fixed h, valid pairs satisfy i <= d-h+1 and j = i+h-1 ranges from h..d
  for (h in 1:m) {
    i_max <- d - h + 1
    if (i_max <= 0) break
    g[h:d] <- g[h:d] + (k[h] * f[1:i_max]) / S_i[1:i_max]
  }
  g
}

# -----------------------
# MCA iteration for n samples (same conceptual role as b %*% M^n)
# -----------------------
f <- c(1, rep(0, d - 1))

for (iter in 1:n) {
  g <- step_band_rowNorm(f, k, S_i)
  g[g < 0] <- 0
  sg <- sum(g)
  if (sg > 0) {
    f <- g / sg
  } else {
    f[] <- 0
    f[1] <- 1
  }
}

fY <- f
FY <- cumsum(fY)

# -----------------------
# Control limits and ARL0 (as in your original)
# -----------------------
# Requires weighted.quantile to be available in your environment.
# If needed, ensure the package providing it is loaded.
CL <- weighted.quantile(State[, 3], fY, probs = c(pnorm(-3), 1 - pnorm(-3)))

LCLy <- State[, 3][which.min(abs(State[, 3] - CL[1]))]
UCLy <- State[, 3][which.min(abs(State[, 3] - CL[2]))]

aux1 <- (State[, 3] <= LCLy | State[, 3] > UCLy) # alterado sinal de < para <= (ou seja, para calcular o ARL0 tem que pegar a probabilidade acumulada até o próprio LCLy)
ARL0 <- 1 / (sum(aux1 * fY))


cat('d =', d, "\n")
cat('LCLx =', LCLy / n, "\n")
cat('UCLx =', UCLy / n, "\n")
cat('LCLy =', LCLy, "\n")
cat('UCLy =', UCLy, "\n")
cat('ARL0 =', ARL0, "\n")
cat('Delta =', delta, "\n")
#cat('jmax =', jmax, "\n")

####### OUT - OF - CONTROL ########
if (!is.null(k_1)) {
  S0 <- 0
  Sd <- Ly_1
  
  # Choose truncation behavior for kernel k:
  use_jmax <- (d > 1000)     # jmax use only for "usual" cases
  #p_target <- 1 - 1e-11      # used only if you decide to use p-target stopping (optional)
  
  # -----------------------
  # Build states
  # -----------------------
  delta <- Ly_1 / d
  S_i <- seq(S0, Sd, by = delta)
  
  State <- rbind(S_i[1:d], S_i[2:(d + 1)])
  State <- t(State)
  E_m <- (State[, 1] + State[, 2]) / 2
  State <- cbind(State, E_m)         # State[,3] is midpoint
  
  # -----------------------
  # Build kernel k = first row of the (unnormalized) Toeplitz-upper-triangular M
  # k[h] corresponds to M[1, 1+h-1] for h=1..d (then optionally truncated to jmax)
  # -----------------------
  k <- numeric(d)
  jmax <- d
  
  for (j in 1:d) {
    dif_E <- State[j, 3] - State[1, 3]
    hi <- dif_E + 0.5 * delta
    lo <- dif_E - 0.5 * delta
    
    # Bin probability (same as your original formula)
    k[j] <- pweibull(hi, shape = k_1, scale = lambda_1) -
      pweibull(lo, shape = k_1, scale = lambda_1)
    
    # Guard against tiny negative values due to rounding/cancellation
    if (k[j] < 0) k[j] <- 0
    
    # Optional: stop by exact underflow-to-zero (not robust for very small d)
    if (use_jmax && k[j] == 0) { jmax <- j - 1; break }
  }
  
  if (jmax < 1) jmax <- 1
  k <- k[1:jmax]
  m <- length(k)
  
  # -----------------------
  # Precompute row-normalization denominators S_i (equivalent to rowSums(M) in original)
  # For row i (1..d), only h=1..min(m, d-i+1) are available (columns cannot exceed d):
  #   S_i = sum_{h=1..min(m, d-i+1)} k[h]
  # -----------------------
  cs <- cumsum(k)                         # cs[h] = sum_{r=1..h} k[r]
  m_i <- pmin(d:1, m)                     # m_i[i] = min(m, d-i+1)
  S_i <- cs[m_i]                          # S_i[i] = sum(k[1:m_i[i]])
  
  # Safety: avoid division by zero (should not happen if k[1]>0)
  S_i[S_i <= 0] <- 1
  
  # -----------------------
  # One-step update reproducing original row normalization:
  #   g[j] = sum_{i=1..j} f[i] * ( k[j-i+1] / S_i[i] )
  # Implemented as banded Toeplitz accumulation (O(d*m))
  # -----------------------
  step_band_rowNorm <- function(f, k, S_i) {
    d <- length(f)
    m <- length(k)
    g <- numeric(d)
    
    # h = j - i + 1  (kernel index)
    # for a fixed h, valid pairs satisfy i <= d-h+1 and j = i+h-1 ranges from h..d
    for (h in 1:m) {
      i_max <- d - h + 1
      if (i_max <= 0) break
      g[h:d] <- g[h:d] + (k[h] * f[1:i_max]) / S_i[1:i_max]
    }
    g
  }
  
  # -----------------------
  # MCA iteration for n samples (same conceptual role as b %*% M^n)
  # -----------------------
  f <- c(1, rep(0, d - 1))
  
  for (iter in 1:n) {
    g <- step_band_rowNorm(f, k, S_i)
    g[g < 0] <- 0
    sg <- sum(g)
    if (sg > 0) {
      f <- g / sg
    } else {
      f[] <- 0
      f[1] <- 1
    }
  }
  
  fY <- f
  FY <- cumsum(fY)
  
  aux1 <- (State[, 3] <= LCLy | State[, 3] > UCLy) 
  ARL1 <- 1 / (sum(aux1 * fY))
  
  cat('ARL1 =', ARL1, "\n")
  
}

cat("Time:", as.double(Sys.time()) - as.double(t0), "s\n")

beepr::beep(5)
