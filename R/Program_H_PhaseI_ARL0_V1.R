# Program H: Effect of Phase I parameter estimation on control limits
# and conditional in-control ARL, using MCA-FFT.
# Default example: Section IX-B, Table XIII (10,000 replications per Phase I size).

rm(list = ls())
t0 <- Sys.time()
options(digits = 6)
library(parallel)                     # replicates on several cores

# ============================================================
# 1. INPUTS
# ============================================================
lambda0 <- 3.2                        # true Weibull scale (application)
k0      <- 4.8                        # true Weibull shape (application)
n       <- 5                          # subgroup size
d       <- 100000                     # number of states (as in the application)
m_set   <- c(20, 50, 100, 200, 300, 500, 1000, 2000, 5000)  # Phase I sizes
R       <- 10000                      # replicates per Phase I size
p_Ly    <- 1 - 1e-11                  # P(Y <= Ly), as in Program D
stable  <- FALSE                       # TRUE: kernel by log-space differencing (no cancellation)

#============================================================
  # 2. FUNCTIONS (MCA-FFT, limits and signal probability as in Program G;
  #    L_y as in Program D)
  # ============================================================
# ------------------------------------------------------------
# FFT convolution of the M* kernel, followed by truncation and renormalisation.
# Returns fY, the midpoints, m, and the mass beyond Ly that is
# discarded before renormalisation.
# ------------------------------------------------------------

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
  f <- Re(fft(fft(c(q, rep(0, N - m)))^n, inverse = TRUE)) / N
  trunc <- sum(f[-(1:d)])                          # mass beyond Ly (discarded)
  f <- f[1:d]
  f[f < 1e-15 * max(f)] <- 0                       # remove FFT round-off
  f <- f / sum(f)
  
  list(fY = f, mid = (1:d - 0.5) * delta, m = m, trunc = trunc)
}

# ------------------------------------------------------------
# Control limits for ARL0 = 1 / (2 * pnorm(-3)) = 370.4:
# weighted quantiles of the midpoints (linear interpolation of
# the c.d.f.), snapped to the nearest midpoint.
# ------------------------------------------------------------
mca_limits <- function(fY, mid) {
  d  <- length(fY)
  FY <- cumsum(fY)
  pr <- c(pnorm(-3), 1 - pnorm(-3))
  CL <- sapply(pr, function(p) {
    l <- max(c(1, which(FY <= p)))
    if (FY[l] < p && l < d) mid[l] + (mid[l + 1] - mid[l]) * (p - FY[l]) / (FY[l + 1] - FY[l]) else mid[l]
  })
  c(LCL = mid[which.min(abs(mid - CL[1]))],
    UCL = mid[which.min(abs(mid - CL[2]))])
}

# ------------------------------------------------------------
# Signal probability P(Y <= LCL or Y > UCL); ARL = 1 / theta.
# LCL and UCL may be any real values (here, midpoints of the grid
# built from the estimates), evaluated on the true-parameter grid.
# ------------------------------------------------------------
mca_theta <- function(fY, mid, LCL, UCL) sum(fY[mid <= LCL | mid > UCL])

# ------------------------------------------------------------
# Upper bound L_y: same rule and numerical settings as Program D
# (minimum of the inclusion and Chernoff)
# ------------------------------------------------------------
Ly_D <- function(n, scale, shape, p) {
  Ly_I     <- n * qweibull(p^(1 / n), shape = shape, scale = scale)
#  Ly_naive <- n * scale * (-log(1 - p))^(1 / shape)
  if (shape > 1) {
    upper <- qweibull(1 - 1e-7, shape = shape, scale = scale)
    MGF_trunc <- function(t) integrate(function(x)
      exp(dweibull(x, shape = shape, scale = scale, log = TRUE) + t * x),
      0, upper, rel.tol = 1e-8, stop.on.error = FALSE)$value
    Ly_C <- optimize(function(t) (n * log(MGF_trunc(t)) - log(1 - p)) / t,
                     interval = c(1e-8, 10))$objective
  } else {
    Ly_C <- Inf
  }
#  min(Ly_I, Ly_C, Ly_naive)
  min(Ly_I, Ly_C)
}

# ============================================================
# 3. DISTRIBUTION OF Y UNDER THE TRUE PARAMETERS (computed once)
# ============================================================
Ly_true <- Ly_D(n, lambda0, k0, p_Ly)
res0  <- mca_fft(d, n, Ly_true, k0, lambda0, stable)
CL0   <- mca_limits(res0$fY, res0$mid)
theta_inf <- mca_theta(res0$fY, res0$mid, CL0["LCL"], CL0["UCL"])
cat("stable =", stable, "\n")
cat("Ly (true parameters) =", Ly_true, "\n")
cat("Known parameters: LCLy =", CL0["LCL"], " UCLy =", CL0["UCL"],
    " ARL0 =", 1 / theta_inf, "\n\n")

# ============================================================
# 4. SUMMARY FUNCTIONS
# ============================================================
summarise <- function(m_I, theta, L, U) {
  carl <- 1 / theta
  data.frame(
    m_I = m_I,
    LCL_mean = mean(L), LCL_sd = sd(L), UCL_mean = mean(U), UCL_sd = sd(U),
    AARL0  = mean(carl),
    SDARL0 = if (length(carl) > 1) sd(carl) else 0,
    CARL_p05 = quantile(carl, 0.05, names = FALSE),
    CARL_p50 = quantile(carl, 0.50, names = FALSE),
    CARL_p95 = quantile(carl, 0.95, names = FALSE),
    CARL_lt200 = mean(carl < 200))
}

# ============================================================
# 5. SIMULATION OVER PHASE I SIZES
# ============================================================
# ------------------------------------------------------------
# Weibull ML estimates by the profile likelihood equation in k:
#   sum(x^k log x) / sum(x^k) - 1/k - mean(log x) = 0,
#   lambda = (mean(x^k))^(1/k).
# The shape equation is solved in one dimension. Rescaling x by max(x)
# avoids overflow without changing the ratio in the equation.
# ------------------------------------------------------------

weibull_ml <- function(x) {
  lx  <- log(x)
  c0  <- max(x)
  ls  <- lx - log(c0)                            # log(x / c0) <= 0
  mlx <- mean(lx)
  g <- function(k) { w <- exp(k * ls); sum(w * lx) / sum(w) - 1 / k - mlx }
  k <- uniroot(g, c(0.05, 50), extendInt = "upX", tol = 1e-10)$root
  c(shape = k, scale = c0 * mean(exp(k * ls))^(1 / k))
}

# ------------------------------------------------------------
# One Phase I replicate: returns LCL, UCL, theta and the mass
# beyond the L_y computed from the estimates
# ------------------------------------------------------------
one_rep <- function(r, m_I) {
  x    <- rweibull(m_I * n, shape = k0, scale = lambda0)
  est  <- weibull_ml(x)
  sh   <- unname(est["shape"]); sc <- unname(est["scale"])
  Ly_r <- Ly_D(n, sc, sh, p_Ly)                  # L_y from the estimates
  res  <- mca_fft(d, n, Ly_r, sh, sc, stable)
  CL   <- mca_limits(res$fY, res$mid)
  LCL  <- unname(CL["LCL"]); UCL <- unname(CL["UCL"])
  c(L = LCL, U = UCL,
    theta = mca_theta(res0$fY, res0$mid, LCL, UCL),
    trunc = res$trunc)
}

# ------------------------------------------------------------
# Replicates run in parallel (one R process per core); the
# parallel RNG streams make the results reproducible
# ------------------------------------------------------------
n_cores <- max(1, detectCores() - 1)
cat("Cores used:", n_cores, "\n")
cl <- makeCluster(n_cores)
clusterExport(cl, c("lambda0", "k0", "n", "d", "p_Ly", "stable", "res0",
                    "mca_fft", "mca_limits", "mca_theta", "Ly_D",
                    "weibull_ml", "one_rep"))
clusterSetRNGStream(cl, 20260928)

out <- NULL
max_trunc <- 0                                   # largest mass beyond each Ly
n_UCL_out <- 0                                   # UCL beyond the true-parameter grid
for (m_I in m_set) {
  t1  <- Sys.time()
  sim <- parSapply(cl, 1:R, one_rep, m_I = m_I)  # 4 x R matrix
  L <- sim["L", ]; U <- sim["U", ]; theta <- sim["theta", ]
  max_trunc <- max(max_trunc, sim["trunc", ])
  n_UCL_out <- n_UCL_out + sum(U > Ly_true)
  out <- rbind(out, summarise(m_I, theta, L, U))
  cat("m_I =", m_I, " time:", round(as.double(Sys.time()) - as.double(t1), 1), "s\n")
}
stopCluster(cl)
out <- rbind(out, summarise(Inf, theta_inf, CL0["LCL"], CL0["UCL"]))  # known parameters
out$LCL_sd[nrow(out)] <- 0; out$UCL_sd[nrow(out)] <- 0

# ============================================================
# 6. OUTPUT
# ============================================================
cat("\nEstimated limits and conditional in-control ARL (stable =", stable, ")\n")
print(format(out[, c("m_I", "LCL_mean", "LCL_sd", "UCL_mean", "UCL_sd",
                     "AARL0", "SDARL0",
                     "CARL_p05", "CARL_p50", "CARL_p95", "CARL_lt200")],
             digits = 4, nsmall = 1), row.names = FALSE)
cat("\nLargest probability mass beyond Ly over all replicates:", max_trunc, "\n")
cat("Replicates with UCL beyond Ly (true parameters):", n_UCL_out, "\n")
cat("Total time:", round(as.double(Sys.time()) - as.double(t0), 1), "s\n")
