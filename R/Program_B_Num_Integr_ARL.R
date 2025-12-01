library(pracma)
tic()
#clear()
#In-Control
#UCL=19.0763 #Calculated with program A    #se acabo de usar o A, a variável já está definida
#LCL=9.83055 #Calculated with program A
#out-of-control
k=5.4 #shape
lambda=2.2 #scale

f <- function(v, w, z, y, x){
  dweibull(x, k,lambda)* dweibull (y, k,lambda)* dweibull (z, k,lambda)* dweibull (w, k,lambda)* dweibull (v, k,lambda)
}
L=LCL
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


L=UCL

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

ARL1=(1/(T1a$value+(1-T2a$value)))

cat('UCL=',UCL,"\n")
cat('LCL=',LCL,"\n")
cat('ARL1=',ARL1,"\n")
toc()

