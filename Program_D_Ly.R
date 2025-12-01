#Ly Limit Determination
n <- 5 #Sample size
k <- 4.8 #shape in-control
lambda <- 3.2 #scale in-control
p_target<-1-(10^(-11)) #P(Y<=Ly) aprox p_target

inclusion=qweibull(p_target^(1/n),shape=k,scale=lambda)
Linclusion=n*inclusion #See Equation 6 of the paper

Naive=lambda*(-log(1-p_target))^(1/k)
LNaive=n*Naive
if (k>1){ #See Equation 7 of the paper
  alpha <- 1-p_target
  MGF_trunc <- function(s, k, lambda){
    # To avoid numerical overflow, the integration was limited to a 
    #sufficiently high quantile rather than extending to infinity
    upper <- qweibull(1 - 1e-12, shape=k, scale=lambda)
    integrand <- function(x){
      # log-stable evaluation
      logf <- dweibull(x, shape=k, scale=lambda, log=TRUE)
      val <- exp(logf + s * x)
      val
    }
    integrate(integrand, 0, upper, rel.tol=1e-8, stop.on.error=FALSE)$value
  }
  
  t_of_s <- function(s) {
    M <- MGF_trunc(s, k, lambda)
    (n * log(M) - log(alpha)) / s
  }
  
  opt <- optimize(t_of_s, interval = c(1e-8, 10))
  s_star <- opt$minimum
  t_chern <- opt$objective
  s_star; t_chern
  Lcher=t_chern
}else{
  Lcher=Inf
}

Limites<-c(Linclusion,Lcher,LNaive)
Lfinal <- min(Limites)
cat("Ly Limit Naive:", ceiling(LNaive), "\n")
cat("Ly Limit inclusion:", ceiling(Linclusion), "\n")
cat("Ly Limit Chernoff: ", 
    if (is.infinite(Lcher)) "Not Applicable" else ceiling(Lcher),
    "\n")

cat("Final Limit for Ly:", ceiling(Lfinal), "\n")
if (Lfinal == LNaive) {
  cat("Note: The Naive limit was the smallest. Compare it with the 
      inclusion-based limit to ensure adequacy.\n")
}
