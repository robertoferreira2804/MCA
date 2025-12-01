library(pracma)
tic()
#clear()
k=5.4 #shape
lambda=2.2 #scale
target_alpha<-0.002699796 #Desired significance level

lower_target_alpha<-target_alpha/2
upper_target_alpha<-1-target_alpha/2

f <- function(v, w, z, y, x){
  dweibull(x, k,lambda)* dweibull (y, k,lambda)* dweibull (z, k,lambda)* dweibull (w, k,lambda)* 
    dweibull (v, k,lambda)
}
OtiA<-function(U) {
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
  
  return((T1a$value-lower_target_alpha)^2)
}

#Refine the interval to a more effective value
resultadoA <- optimize(OtiA, interval = c(6.9, 7.3))  
LCL=resultadoA$minimum

OtiB<-function(U) {
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
  
  
  return((T2a$value-upper_target_alpha)^2)
}

#Refine the interval to a more effective value
resultadoB <- optimize(OtiB, interval = c(12.6, 13.1))  
UCL=resultadoB$minimum

cat('UCLy=',UCL,"\n")
cat('LCLy=',LCL,"\n")
toc()

