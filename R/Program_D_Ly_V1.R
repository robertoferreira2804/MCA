#Ly Limit Determination
# Symbolic correspondence with the paper:
# X ~ Weibull(lambda_0, k_0)
# Y = sum_{i=1}^n X_i
# Ly_I      -> inclusion-based upper bound (L_y^I)
# Ly_naive -> naive upper bound (L_y^{naive})
# Ly_C     -> Chernoff upper bound (L_y^C)
# Ly -> final bound used in the MCA

rm(list = ls())
n <- 5 #Sample size
lambda_0 <- 4 #scale in-control
k_0 <- 5 #shape in-control
p <- 1 - 1e-11 #P(Y<=Ly) ~ p

Lx  <-  qweibull(p^(1/n), shape = k_0, scale = lambda_0)
Ly_I  <- n * Lx #See Equation 10 (in 4.1. (1.)) of the paper

Lx_star  <- lambda_0 * (-log(1-p)) ^ (1/k_0)
Ly_naive <- n * Lx_star

if (k_0 > 1){ #See Equation 11 (in 4.1. (2.)) of the paper
 
  MGF_trunc <- function(t, k_0, lambda_0){
    # To avoid numerical overflow, the integration was limited to a 
    #sufficiently high quantile rather than extending to infinity
    upper <- qweibull(1 - 1e-7, shape=k_0, scale=lambda_0)
    integrand <- function(x){
      # log-stable evaluation
      logf <- dweibull(x, shape=k_0, scale=lambda_0, log=TRUE)
      val <- exp(logf + t * x)
      val
    }
    integrate(integrand, 0, upper, rel.tol=1e-8, stop.on.error=FALSE)$value
  }
  
  LyC_of_t <- function(t) {
    M <- MGF_trunc(t, k_0, lambda_0)
    (n * log(M) - log(1 - p)) / t
  }
  
  opt <- optimize(LyC_of_t, interval = c(1e-8, 10))
  t_star <- opt$minimum
  Ly_C <- opt$objective
  t_star; Ly_C
  
}else{
  Ly_C  <- Inf
}

Limites<-c(Ly_I,Ly_C,Ly_naive)
Ly <- min(Limites)
cat(sprintf("Ly -> Limit Naive     = %.1f\n", Ly_naive))
cat(sprintf("Ly -> Limit inclusion = %.1f\n", Ly_I))
cat(sprintf("Ly -> Limit Chernoff  = %.1f\n", Ly_C))
cat(sprintf("Ly -> Final limit     = %.1f\n", Ly))

if (Ly == Ly_naive) {
  cat("Note: The Naive limit was the smallest. Compare it with the 
      inclusion-based limit to ensure adequacy.\n")
}
