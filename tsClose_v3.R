# https://www.youtube.com/watch?v=l08LICz8Ink
# https://www.youtube.com/watch?v=v0sivj2wGcA

# Cambiar directorio actual
setwd("g:/2021 MASTER/TFM/Datos")

# Cargar fichero cotizaciones diarias Bitcoin
library(readr)
BTC_USD <- read_csv("BTC-USD.csv")
SP500 <- read_csv("sp500_2.csv")
r <- NULL


#Trabajar con el dataframe 
#attach(SP500)

#Visualizar cinco primeras filas
#head(close)

library(pracma)
hurstexp(SP500$close,d=4, display=TRUE)


hurst_rs <- function(v) {

  #Calculo de parametros
  n <- length(v)-1   #longitud del vector de retornos 
                     #(1 menos del vector original)(n)
  #Vector primera diferencia
  v2 <- rep(0,n)
  for(i in 1:n) {
    v2[i] = (v[i+1]/v[i])-1
  }

  indice <- 1:n  #vector auxiliar de indices (de 1 a longitud)
  
  #calculando el numero k elementos para la regresion
  k <- 0
  repeat {
    k = k+1
    if((2^k)>=n) {
      break
    }
  }
  k=k-1
  nk = k
  
  #vectores para regresion
  vector_rs = matrix(rep(0,nk))     #vector rango reescalado
  vector_n = matrix(rep(0,nk))      #vector numero elementos
  vector_log_rs = matrix(rep(0,nk)) #vector logaritmo rango reescalado
  vector_log_n = matrix(rep(0,nk))  #vector logaritmo numero elementos
  
  
  #recorrer los k puntos de la regresion
  for(k in 1:(nk)){
    m = 2^k  #numero de elementos en cada subconjunto
    d = floor(n/m)  #numero de bloques
    if(d==1) d=2 #comprobación punto final 
                 #(si el segundo subconjunto es menor de 2^k, d será 1, 
                 #y dará error numérico)
    
    vector_indices = matrix(rep(0,n)) #vector de indices
    vector_normalizado = matrix(rep(1,n))
    vector_acumulado = matrix(rep(1,n))
    vector_rango_RS = matrix(rep(1,d-1))
    vector_prueba = matrix(rep(1,n))
    
    for(i in 1:n){
      vector_indices[i]=(i-1)%/%(2^k)
    }
    
    i=0
    j=0
    repeat {
      i=i+1
      if(i>n) break
      vector_normalizado[i] <- (v2[i]-mean(subset(v2,vector_indices==j)))#/sd(subset(v2,vector_indices==j))
      vector_prueba[i] = mean(subset(v2,vector_indices==j))
      if((i)%%(2^k)==0) {j = j+1}
    }

    i=0
    vector_acumulado <- NULL
    repeat {
      vector_acumulado <- c(vector_acumulado,cumsum(subset(vector_normalizado,vector_indices==i)))
      i=i+1
      if(i>d) break
    }
    vector_acumulado <- matrix(vector_acumulado)
    
    i=0
    #vector_rango <- NULL
    vector_rango_RS <- matrix(rep(0,d))
    repeat {
      vector_rango_RS[i+1] <- (max(subset(vector_acumulado,vector_indices==i)) - 
                               min(subset(vector_acumulado,vector_indices==i)))/
                               sd(subset(v2,vector_indices==i))
      #print(i)
      i=i+1
      if(i>=d) break
    }

    vector_rs[k] = mean(vector_rango_RS)
    vector_log_rs[k] = log(vector_rs[k])
    vector_n[k] = 2^k
    vector_log_n[k] = log(vector_n[k])
  }
  
  
  
  #Calculo de la regresion
  plot(vector_log_n,vector_log_rs)
  regresion <- lm(vector_log_rs~vector_log_n)
  r <- regresion
  df <- data.frame(vector_log_n,vector_log_rs)
  print(df)
  return(regresion$coefficients[2])
  
}

hurst_rs(SP500$close)
df

hurst_rs(BTC_USD$Close)
df

plot(SP500$close)
