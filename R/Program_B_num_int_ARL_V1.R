
#rm(list = ls())
t0 <- Sys.time()

#In-Control
#UCLy <- 12.85017 #Calculated with program A    #same variables from program A
#LCLy <- 7.094406 #Calculated with program A

#out-of-control
lambda_1 <- 1.9 #scale on H1 (out of control)
k_1 <- 5.4  #shape on H1 (out of control)

# to calculate ARL_0, use lambda_0 and k_0 values:
#lambda_1 <- 2.2 #scale on H0 (to calculate ARL_0)
#k_1 <- 5.4  #shape on H0 (out of control)

f <- function(x, y, z, w, v){
  dweibull(x, k_1,lambda_1)* dweibull (y, k_1,lambda_1)* dweibull (z, k_1,lambda_1)* dweibull (w, k_1,lambda_1)* dweibull (v, k_1,lambda_1)
}
L <- LCLy
T1a<-integrate(Vectorize(function(x){
  integrate(Vectorize(function(y){
    integrate(Vectorize(function(z){
      integrate(Vectorize(function(w){
        integrate(function(v){
          f(x, y, z, w, v)
        }, lower = 0, upper = L-x-y-z-w)$value
      }), lower = 0, upper = L-x-y-z)$value
    }), lower = 0, upper = L-x-y)$value
  }), lower = 0, upper = L-x)$value
}), lower = 0, upper = L)


L <-UCLy
T2a<-integrate(Vectorize(function(x){
  integrate(Vectorize(function(y){
    integrate(Vectorize(function(z){
      integrate(Vectorize(function(w){
        integrate(function(v){
          f(x, y, z, w, v)
        }, lower = 0, upper = L-x-y-z-w)$value
      }), lower = 0, upper = L-x-y-z)$value
    }), lower = 0, upper = L-x-y)$value
  }), lower = 0, upper = L-x)$value
}), lower = 0, upper = L)

ARL_1=(1/(T1a$value+(1-T2a$value)))

cat('UCLy = ', UCLy,"\n")
cat('LCLy = ', LCLy,"\n")
cat('ARL_1 = ', ARL_1,"\n")

Sys.time() - t0

