rm(list = ls())
t0 <- Sys.time()

lambda_0 <- 2.2 #scale on H0 (in-control)
k_0 <- 5.4 #shape on H0 (in-control)
alpha <- 2 * (1 - pnorm(3)) # Desired significance level = 0.002699796

lower_alpha <- alpha / 2
upper_alpha <- 1 - alpha / 2

f <- function(x, y, z, w, v){
  dweibull(x, k_0,lambda_0) * dweibull(y, k_0,lambda_0) * dweibull(z, k_0,lambda_0) * dweibull(w, k_0,lambda_0) * 
    dweibull(v, k_0,lambda_0)
}
OtiA<-function(U) {     # for n = 5
  L=U[1]
  T1a<-integrate(Vectorize(function(x){
         integrate(Vectorize(function(y){
           integrate(Vectorize(function(z){
             integrate(Vectorize(function(w){
               integrate(function(v){
                f(x, y, z, w, v)
               }, lower = 0, upper = L-x-y-z-w)$value
             }), lower = 0, upper = L-x-y-z)$value
           }), lower = 0, upper =  L-x-y)$value
         }), lower = 0, upper =  L-x)$value
       }), lower = 0, upper = L)
  
  return((T1a$value - lower_alpha)^2)
}

#Refine the interval to a more effective value
resultadoA <- optimize(OtiA, interval = c(6.9, 7.3))  
LCLy=resultadoA$minimum

OtiB<-function(U) {     # for n = 5
  L=U[1]
  
  T2a<-integrate(Vectorize(function(x){
         integrate(Vectorize(function(y){
           integrate(Vectorize(function(z){
             integrate(Vectorize(function(w){
               integrate(function(v){
                f(x, y, z, w, v)
               }, lower = 0, upper = L-x-y-z-w)$value
             }), lower = 0, upper = L-x-y-z)$value
           }), lower = 0, upper =  L-x-y)$value
         }), lower = 0, upper =  L-x)$value
       }), lower = 0, upper = L)
  
  
  return((T2a$value - upper_alpha)^2)
}

#Refine the interval to a more effective value
resultadoB <- optimize(OtiB, interval = c(12.6, 13.1))  
UCLy=resultadoB$minimum

cat('UCLy=',UCLy,"\n")
cat('LCLy=',LCLy,"\n")

Sys.time() - t0

