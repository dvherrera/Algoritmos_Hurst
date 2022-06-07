#Simulación movimiento browniano

BM <- function(Tiempo=10, pasos=0.01, mu=0, sigma=1) {
  t <- seq(0,Tiempo,pasos)
  for(i in 1:(length(t)-1)) {
    Z <- rnorm(1)
    x[i+1] <- x[i] + sigma*sqrt(t[i+1]-t[i])*Z + mu*(t[i+1]-t[i])
  }
  data.frame(t,x)
}

Q <- BM(15,0.01,0,1)
library(ggplot2)
ggplot(Q, aes(x=t, y=x)) + geom_line()
Q <- BM(10,0.01,0,1)




set.seed(100)
dt = 0.01

T = seq(-50,10,0.1)

n=length(T)
m=length(T)
Gamma = matrix(0, n, m)

H=0.9
exponent <- function(a, pow) (abs(a)^pow)*sign(a)


for(j in 1:length(T)){
  zeile = numeric(length(T)) #resetting our path for each j
  y = sqrt(dt)*rnorm(n=1, mean = 0, sd=1) #normal distributred r.V.
  zeile[1] = (max(exponent(T[j] - T[1],H-0.5),0) - max(exponent(-T[1],H-0.5),0))*y #first entry of one path
  for(i in 1:(length(T)-1)){
    y1 = sqrt(dt)*rnorm(n=1, mean = 0, sd=1)
    y2 = sqrt(dt)*rnorm(n=1, mean = 0, sd=1)
    zeile[i+1] = zeile[i] + max(exponent(T[j] - T[i],H-0.5),0)*y1 - max(exponent(-T[i],H-0.5),0)*y2
  }
  Gamma[j,] = zeile
}

normalV = rnorm(length(T),mean =0,sd=1)
path = Gamma%*%normalV

plot(T, path, type="l")
plot(T, cumsum(path), type="l")



t <- 0:1000
sig2 <- 0.01
## en primer lugar, simulamos una serie de desviaciones aleatorias
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
x <- c(0,cumsum(x))
plot(t, x, type="l", ylim=c(-10,10))


#https://cran.r-project.org/web/packages/somebm/somebm.pdf
#https://github.com/732jhy/fractionalBM
#https://stackoverflow.com/questions/72014423/simulating-fractional-brownian-motion
#https://es.mathworks.com/matlabcentral/fileexchange/38935-fractional-brownian-motion-generator
#https://rdrr.io/cran/somebm/man/fbm.html
#https://www.intechopen.com/chapters/43535


