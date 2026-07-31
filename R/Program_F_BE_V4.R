library(pracma)
rm(list = ls())
gc(reset = TRUE)
tic()
options(digits = 8)

ARL0_target <- 1/(2*pnorm(-3))  # (~ 370.4)

d <- 1000
n <- 5
lambda0 <- 4
k0 <- 5
Ly  <- 29.5555
Delta <- Ly / d
# Controls the batch size for building the transition matrix.
# Affects runtime and peak memory usage, but not the final results.
chunk <- 1000

# ---- State grid ----
state_grid <- seq(0, Ly, by = Delta)
tv <- length(state_grid)
IE <- state_grid[1:(tv - 1)]
FE <- state_grid[2:tv]
ME <- (IE + FE) / 2

Ii_block1 <- pweibull(FE, k0, lambda0) - pweibull(IE, k0, lambda0)
S1 <- sum(Ii_block1)
Ii_last <- 1 - S1

# ---- Transition matrix (built only once) ----
Bblock <- matrix(0, d, d)
row_starts <- seq(1, d, by = chunk)
for (start in row_starts) {
  end <- min(start + chunk - 1, d)
  rows <- start:end
  Diff <- outer(ME[rows], ME, function(a, b) b - a)
  RES1 <- pweibull(Diff + Delta/2, k0, lambda0)
  RES3 <- pweibull(Diff - Delta/2, k0, lambda0)
  row_block <- RES1 - RES3
  row_block[Diff < 0] <- 0
  Bblock[rows, ] <- row_block
}
rm(Diff, RES1, RES3, row_block)

# Bblock_raw : NOT normalized -> used in Brook-Evans (ARL evaluator)
Bblock_raw <- Bblock

# Bblock_norm: normalized (fixrows) -> used ONLY to reconstruct Y
Bblock_norm <- Bblock
rs <- rowSums(Bblock_norm)
fixrows <- which(rs < 1)
for (j in fixrows) {
  k1 <- max(which(Bblock_norm[j, ] > 0))
  Bblock_norm[j, k1] <- Bblock_norm[j, k1] + (1 - rs[j])
}

# =================================================================
# CONSTRAINT: reconstruction of Y (normalized) -> defines the
# (LCL(alpha), UCL(alpha)) pairs with equal tails
# =================================================================
vY <- Ii_block1
for (k in 2:n) vY <- as.numeric(t(Bblock_norm) %*% vY)
vY[d] <- vY[d] + Ii_last
cumY <- cumsum(vY)

# index i -> LCL = ME[i] (lower tail = cumY[i])
# index j -> UCL = ME[j], chosen to match the upper tail
UCL_index_for <- function(i) {
  target_p <- 1 - cumY[i]
  w <- which(cumY >= target_p)
  if (length(w) == 0) return(d)
  w[1]
}

# =================================================================
# EVALUATOR: ARL via Brook-Evans (NOT normalized)
# Kept for spot-check validation (see the verification block at the end)
# =================================================================
ARL_Final <- function(i, j) {
  LCL <- ME[i]; UCL <- ME[j]
  indicIC <- as.numeric(ME >= LCL & ME <= UCL)
  a_next <- indicIC
  b_next <- indicIC
  for (k in (n - 1):1) {
    a_next <- 1 + as.numeric(Bblock_raw %*% a_next)
    b_next <- as.numeric(Bblock_raw %*% b_next)
  }
  num <- 1 + sum(Ii_block1 * a_next) + Ii_last * indicIC[d]
  den <- 1 - (sum(Ii_block1 * b_next) + Ii_last * indicIC[d])
  (num / den) / (n + 1)
}

# =================================================================
# STEP 1 (implementation): uniroot() just to center the search
# =================================================================
quantY_idx <- function(p) which(cumY >= p)[1]
f_alpha <- function(alpha) {
  i <- quantY_idx(alpha / 2)
  j <- UCL_index_for(i)
  ARL_Final(i, j) - ARL0_target
}
sol <- uniroot(f_alpha, interval = c(1e-6, 0.5), tol = 1e-8)
i_center <- quantY_idx(sol$root / 2)

# =================================================================
# STEP 2 (vectorized): the same 121 candidates and the same ARL_Final
# equation as the original version, but ALL propagated AT ONCE via
# matrix-matrix products (GEMM), instead of 121 calls to
# ARL_Final(i,j), each with (n-1) matrix-vector products (GEMV).
# =================================================================
# Search radius (in states) around i_center for the candidate optimization.
# Unlike 'chunk', this CAN affect the result if too small: the true
# discrete optimum must lie within [i_center - window, i_center + window].
# Larger window = safer but more computationally expensive.
window <- 60
candidates_i <- seq(max(1, i_center - window), min(d, i_center + window))
ncand <- length(candidates_i)
candidates_j <- vapply(candidates_i, UCL_index_for, integer(1))

# indicIC for each candidate = 1 for ME in [LCL_i, UCL_j] (ME is sorted,
# so it is a contiguous block of rows, same as the indicIC in ARL_Final)
IndicMat <- matrix(0, d, ncand)
for (c in seq_len(ncand)) {
  IndicMat[candidates_i[c]:candidates_j[c], c] <- 1
}

# Stacks a_next and b_next into a single d x (2*ncand) block: 1
# matrix-matrix product per iteration, instead of 121 x 2 matrix-vector
# products
Mm <- cbind(IndicMat, IndicMat)     # columns 1:ncand = "a", (ncand+1):(2ncand) = "b"
idxA <- 1:ncand
idxB <- (ncand + 1):(2 * ncand)

for (k in (n - 1):1) {
  Mm <- Bblock_raw %*% Mm
  Mm[, idxA] <- Mm[, idxA] + 1       # only a_next receives the "+1", exactly as before
}

a_next_mat <- Mm[, idxA, drop = FALSE]
b_next_mat <- Mm[, idxB, drop = FALSE]

last_row_IC <- IndicMat[d, ]
num <- 1 + as.numeric(Ii_block1 %*% a_next_mat) + Ii_last * last_row_IC
den <- 1 - (as.numeric(Ii_block1 %*% b_next_mat) + Ii_last * last_row_IC)
ARL_vec <- (num / den) / (n + 1)

results <- data.frame(
  i = candidates_i, j = candidates_j,
  ARL = ARL_vec, sq_error = (ARL_vec - ARL0_target)^2
)
best <- results[which.min(results$sq_error), ]

LCLy <- ME[best$i]
UCLy <- ME[best$j]
P_below <- cumY[best$i]
P_above <- 1 - cumY[best$j]

cat("===== FINAL RESULT =====\n")
cat("LCLy =", sprintf('%.10f', LCLy), "\n")
cat("UCLy =", sprintf('%.10f', UCLy), "\n")
cat("P(Y < LCLy) =", P_below, "\n")
cat("P(Y > UCLy) =", P_above, "\n")
cat("Difference between tails =", P_below - P_above, "\n")
cat("ARL_BE at optimum =", best$ARL, " (target:", ARL0_target, ")\n")
cat("Squared error (ARL_BE - target)^2 =", best$sq_error, "\n")
toc()