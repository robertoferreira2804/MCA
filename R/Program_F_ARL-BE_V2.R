ARL_BE_sparse <- function(
    d, n, Ly,
    lambda0, k0,
    LCLy, UCLy,
    epsX = 1e-8,
    return_in_samples = TRUE,
    verbose = TRUE
) {
  stopifnot(d >= 10, n >= 2, Ly > 0, lambda0 > 0, k0 > 0)
  stopifnot(LCLy < UCLy, LCLy >= 0, UCLy <= Ly)
  
  suppressPackageStartupMessages({
    library(Matrix)
  })
  
  # -----------------------------
  # Grid / states (uniform)
  # -----------------------------
  Delta <- Ly / d
  S <- seq(0, Ly, length.out = d + 1)
  mid <- (S[-1] + S[-(d + 1)]) / 2
  
  in_control_mid <- (mid >= LCLy) & (mid <= UCLy)
  
  # -----------------------------
  # Bandwidth via quantile of X
  # -----------------------------
  Lx <- qweibull(1 - epsX, shape = k0, scale = lambda0)
  w <- ceiling(Lx / Delta) + 2
  
  if (verbose) {
    cat("Delta =", Delta, "\n")
    cat("Lx(q)  =", Lx, "with epsX =", epsX, "\n")
    cat("Bandwidth w ~", w, "offsets per row (0..w)\n")
  }
  
  # Transition probs by offset (midpoint window)
  h <- 0:w
  a <- pmax(0, h * Delta - Delta/2)
  b <- h * Delta + Delta/2
  p_h <- pweibull(b, shape = k0, scale = lambda0) - pweibull(a, shape = k0, scale = lambda0)
  p_h[p_h < 0] <- 0
  
  # Initial distribution I -> (i,1): exact interval probs
  p_init <- pweibull(S[-1], shape = k0, scale = lambda0) - pweibull(S[-(d + 1)], shape = k0, scale = lambda0)
  p_init[p_init < 0] <- 0
  tail_init <- 1 - sum(p_init)
  if (tail_init > 0) p_init[d] <- p_init[d] + tail_init
  
  # -----------------------------
  # State indexing and transient mapping
  # -----------------------------
  idx_I <- 1L
  idx_state <- function(i, j) 1L + (j - 1L) * d + i
  
  full_size <- 1L + n * d
  is_transient_full <- rep(FALSE, full_size)
  is_transient_full[idx_I] <- TRUE
  
  for (j in 1:(n - 1)) {
    is_transient_full[1L + (j - 1L) * d + (1:d)] <- TRUE
  }
  is_transient_full[1L + (n - 1L) * d + which(in_control_mid)] <- TRUE
  
  transient_full_idx <- which(is_transient_full)
  s <- length(transient_full_idx)
  map_to_T <- integer(full_size)
  map_to_T[transient_full_idx] <- seq_len(s)
  
  if (verbose) {
    cat("Full chain size (incl I and all blocks) =", full_size, "\n")
    cat("Transient states (dimension s)          =", s, "\n")
    cat("Absorbing states at j=n (count)         =", sum(!in_control_mid), "\n")
  }
  
  # -----------------------------
  # PRE-ALLOCATION (critical fix)
  # nnz estimate ~ I->(i,1): d
  # + within blocks: (n-1)*d*(avg(hmax)+1) ~ (n-1)*d*(min(w,d-1)+1)
  # + final block (in-control): sum(in_control_mid)
  # Add some slack factor.
  # -----------------------------
  w_eff <- min(w, d - 1L)
  nnz_est <- as.integer(
    d + (n - 1L) * d * (w_eff + 1L) + sum(in_control_mid)
  )
  slack <- 1.15
  nnz_cap <- as.integer(ceiling(nnz_est * slack))
  if (nnz_cap < 1000L) nnz_cap <- 1000L
  
  ii <- integer(nnz_cap)
  jj <- integer(nnz_cap)
  xx <- numeric(nnz_cap)
  ptr <- 1L
  
  # Fast appender (no c())
  add_entries <- function(rT, cT, val) {
    k <- length(val)
    if (k == 0) return(invisible(NULL))
    
    end <- ptr + k - 1L
    if (end > length(ii)) {
      # grow capacity (rare if slack is ok)
      new_cap <- as.integer(ceiling(length(ii) * 1.5))
      length(ii) <<- new_cap
      length(jj) <<- new_cap
      length(xx) <<- new_cap
    }
    
    ii[ptr:end] <<- rT
    jj[ptr:end] <<- cT
    xx[ptr:end] <<- val
    ptr <<- end + 1L
    invisible(NULL)
  }
  
  # -----------------------------
  # I -> (i,1)
  # -----------------------------
  rT <- map_to_T[idx_I]
  cols_full <- idx_state(1:d, 1)
  cols_T <- map_to_T[cols_full]
  keep <- cols_T > 0
  add_entries(rep.int(rT, sum(keep)), cols_T[keep], p_init[keep])
  
  # -----------------------------
  # (i,j) -> (k,j+1), j=1..n-1
  # -----------------------------
  for (j in 1:(n - 1)) {
    for (i in 1:d) {
      from_T <- map_to_T[idx_state(i, j)]
      if (from_T == 0L) next
      
      hmax <- min(w, d - i)
      probs <- p_h[1:(hmax + 1)]
      row_sum <- sum(probs)
      if (row_sum < 1) probs[hmax + 1] <- probs[hmax + 1] + (1 - row_sum)
      
      k <- i + (0:hmax)
      to_T <- map_to_T[idx_state(k, j + 1)]
      keep2 <- to_T > 0
      if (any(keep2)) {
        add_entries(rep.int(from_T, sum(keep2)), to_T[keep2], probs[keep2])
      }
    }
  }
  
  # -----------------------------
  # Final block j=n: in-control -> I ; out-of-control excluded (absorbing)
  # -----------------------------
  for (i in which(in_control_mid)) {
    from_T <- map_to_T[idx_state(i, n)]
    if (from_T == 0L) next
    add_entries(from_T, map_to_T[idx_I], 1.0)
  }
  
  # Trim vectors to used length
  used <- ptr - 1L
  ii <- ii[1:used]
  jj <- jj[1:used]
  xx <- xx[1:used]
  
  # -----------------------------
  # Build sparse Q and solve (I - Q) t = 1
  # -----------------------------
  Q <- sparseMatrix(i = ii, j = jj, x = xx, dims = c(s, s))
  t_vec <- solve(Diagonal(s) - Q, rep(1, s))
  ARL_steps <- as.numeric(t_vec[ map_to_T[idx_I] ])
  
  ARL_samples_n <- ARL_steps / n
  ARL_samples_n1 <- ARL_steps / (n + 1)
  
  if (verbose) {
    cat("ARL (in chain steps)        =", ARL_steps, "\n")
    cat("ARL approx (steps / n)      =", ARL_samples_n, "\n")
    cat("ARL approx (steps /(n+1))   =", ARL_samples_n1, "\n")
    cat("nnz used / cap              =", used, "/", nnz_cap, "\n")
  }
  
  if (return_in_samples) {
    return(list(
      ARL_steps = ARL_steps,
      ARL_samples_div_n = ARL_samples_n,
      ARL_samples_div_n1 = ARL_samples_n1,
      Delta = Delta,
      Lx = Lx,
      bandwidth_w = w,
      transient_dim = s,
      full_dim = full_size,
      nnz_used = used,
      nnz_cap = nnz_cap
    ))
  } else {
    return(ARL_steps)
  }
}


####### EXECUTION #####
library(peakRAM)
library(pracma)
{
  tic()
  RAM <- peakRAM({
    t <- system.time({
      res <- ARL_BE_sparse(
        d = 30000,
        n = 5, Ly = 30,     # n=5 ,  Ly = 30
        lambda0 = 4, k0 = 5,
        LCLy = 12.503, UCLy = 23.670,
        epsX = 1e-6,
        verbose = TRUE
      )
    })
    t
    res$ARL_samples_div_n1
  })
  RAM
  toc()
  beepr::beep(5)
  
}
############################################

{
tic()
ds <- c(200, 500, 1000, 2000)
out <- lapply(ds, function(dd) {
  ARL_BE_sparse(
    d = dd, n = 5, Ly = 30,
    lambda0 = 4, k0 = 5,
    LCLy = 12.503, UCLy = 23.672,
    epsX = 1e-6,
    verbose = TRUE
  )$ARL_samples_div_n1
})

data.frame(d = ds, ARL_BE = unlist(out))
toc
}
####################################
Rprof("prof.out")
res <- ARL_BE_sparse(d=1000, n=5, Ly=30, lambda0=4, k0=5, LCLy=12.503, UCLy=23.672, epsX=1e-6, verbose=FALSE)
Rprof(NULL)
summaryRprof("prof.out")
###############################
bench <- function(dd, eps=1e-6){
  t <- system.time({
    r <- ARL_BE_sparse(
      d=dd, n=5, Ly=30,
      lambda0=4, k0=5,
      LCLy=12.503, UCLy=23.672,
      epsX=eps,
      verbose=FALSE
    )
  })
  c(d=dd, epsX=eps, time=t["elapsed"], w=r$bandwidth_w, s=r$transient_dim, nnz=r$nnz_used, ARL=r$ARL_samples_div_n1)
}

rbind(
 bench(2000, 1e-6),
 bench(5000, 1e-6),
 bench(10000, 1e-6),
  bench(15000, 1e-6),
  bench(20000, 1e-6)
)

beepr::beep(5)
####################################