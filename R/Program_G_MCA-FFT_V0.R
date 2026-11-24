# Program G (MCA-FFT): Control limits and ARL for Weibull processes.
# Computes the distribution of the sample sum by FFT convolution of the
# M* transition kernel, then retains the first d states and renormalizes.

rm(list = ls())
t0 <- Sys.time()
options(digits = 6)

# ============================================================
# 1. INPUTS
# ============================================================
d <- 1000           # number of states
n <- 5                 # sample size
stable <- TRUE         # TRUE: kernel by log-space differencing (no cancellation)

# in-control
lambda_0 <- 4.0        # Weibull scale
k_0      <- 5.0        # Weibull shape
Ly_0     <- 29.5555    # upper bound for Y (Program_D_Ly.R, same n)
# didactic example (Tables II-III): d <- 10; Ly_0 <- 30

# out-of-control (k_1 <- NULL to skip ARL1)
k_1      <- NULL          # Weibull shape
lambda_1 <- 3.5        # Weibull scale (NULL to use the mean shift 'sh')
sh       <- -0.05      # relative shift in E[X], used only if lambda_1 = NULL
Ly_1     <- 25.8611    # upper bound for Y (Program_D_Ly.R, same n)

if (!is.null(k_1) && is.null(lambda_1)) {
  lambda_1 <- lambda_0 * gamma(1 + 1 / k_0) * (1 + sh) / gamma(1 + 1 / k_1)
}

# ============================================================
# fY = b %*% M^n by a single FFT (used for both ARL0 and ARL1)
# ============================================================
mca_fft <- function(d, n, Ly, shape, scale, stable = TRUE) {
  delta <- Ly / d
  
  # kernel k = first row of the unnormalised transition matrix M*;
  # the kernel is cut at the first exact zero after its mode
  # (m = number of retained, numerically nonzero entries)
  h  <- 0:(d - 1)
  hi <- h * delta + 0.5 * delta
  lo <- h * delta - 0.5 * delta
  if (stable) {
    # log-space differencing: S(lo) - S(hi) = exp(-A) * (1 - exp(-(B - A))),
    # with 1 - exp(-x) computed by expm1; avoids subtracting two values of
    # the CDF close to one in the upper tail of X
    A <- (pmax(lo, 0) / scale)^shape
    B <- (hi / scale)^shape
    k <- exp(-A) * (-expm1(A - B))
  } else {
    k <- pweibull(hi, shape, scale) - pweibull(lo, shape, scale)
  }
  k[k < 0] <- 0
  j0 <- which.max(k)                             # mode of the kernel
  z  <- which(k[j0:d] == 0)                      # first exact zero after it
  m  <- if (length(z)) j0 + z[1] - 2 else d
  k  <- k[1:m]
  
  q <- k / sum(k)                                  # first row of M (normalised)
  N <- 2^ceiling(log2(max(n * (m - 1) + 1, d)))    # FFT length
  f <- Re(fft(fft(c(q, rep(0, N - m)))^n, inverse = TRUE))[1:d] / N
  f[f < 1e-15 * max(f)] <- 0                       # remove FFT round-off
  f <- f / sum(f)
  
  list(fY = f, mid = (1:d - 0.5) * delta, m = m)
}

# ============================================================
# 2. IN-CONTROL: distribution of Y, control limits, ARL0
# ============================================================
res <- mca_fft(d, n, Ly_0, k_0, lambda_0, stable)
fY  <- res$fY
mid <- res$mid
m_0 <- res$m

# --- control limits: weighted quantiles of the midpoints, snapped to a midpoint ---
FY <- cumsum(fY)
q  <- c(pnorm(-3), 1 - pnorm(-3))
CL <- sapply(q, function(p) {                   # weighted quantile, linear
  l <- max(c(1, which(FY <= p)))                 # interpolation of the c.d.f.
  if (FY[l] < p && l < d) mid[l] + (mid[l + 1] - mid[l]) * (p - FY[l]) / (FY[l + 1] - FY[l]) else mid[l]
})
LCLy <- mid[which.min(abs(mid - CL[1]))]
UCLy <- mid[which.min(abs(mid - CL[2]))]

ARL0 <- 1 / sum(fY[mid <= LCLy | mid > UCLy])

# ============================================================
# 3. OUT-OF-CONTROL: ARL1 with the in-control limits
# ============================================================
if (!is.null(k_1)) {
  res1 <- mca_fft(d, n, Ly_1, k_1, lambda_1, stable)
  fY1  <- res1$fY
  mid1 <- res1$mid
  ARL1 <- 1 / sum(fY1[mid1 <= LCLy | mid1 > UCLy])
}

# ============================================================
# 4. OUTPUT
# ============================================================
cat("method = FFT   stable =", stable, "  d =", d, "  n =", n, "  m =", m_0, "\n")
cat("LCLx =", LCLy / n, "  UCLx =", UCLy / n, "\n")
cat("LCLy =", LCLy, "  UCLy =", UCLy, "\n")
cat("ARL0 =", ARL0, "\n")
cat("Delta =", Ly_0 / d, "\n")
if (!is.null(k_1)) cat("ARL1 =", ARL1, "\n")
cat("Time:", as.double(Sys.time()) - as.double(t0), "s\n")
