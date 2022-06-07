# https://www.youtube.com/watch?v=l08LICz8Ink
# https://www.youtube.com/watch?v=v0sivj2wGcA

# Cambiar directorio actual
setwd("g:/2021 MASTER/TFM/Datos")

# Cargar fichero cotizaciones diarias Bitcoin
library(readr)
BTC_USD <- read_csv("BTC-USD.csv")
SP500 <- read_csv("sp500_2.csv")

#Trabajar con el dataframe 
attach(SP500)

#Visualizar cinco primeras filas
head(close)

#tsClose = ts(Close, frequency=365, start=c(2014,260))
#tsClose
#plot(tsClose)
#summary(tsClose)

#window(tsClose,start=2016,end=2021)
#index(tsClose)

#library(lubridate)
#class(Date)

# Libreria de graficos
library(ggplot2)

# https://statologos.com/trazar-series-de-tiempo-en-r/
# Crear grafico de serie temporal
p <- ggplot(SP500, aes(x=Date, y=close))+geom_line(color="turquoise4")+labs(title="SP500")
p


library(pracma)
hurstexp(close,d=4, display=TRUE)



hurst_rs <- function(v) {
  #Calculo de parametros
  longitud <- length(v)-1   #longitud del vector
  #Vector primera diferencia
  v2 <- rep(0,longitud)
  for(i in 1:longitud) {
    #v2[i] = log(v[i+1]/v[i])   #(v[i+1]/v[i])-1
    v2[i] = (v[i+1]/v[i])-1
  }

  indice <- 0:(longitud-1)    #vector de indices (de 1 a longitud)
  potencia = 1            #variable para numero de potencias de 2 a calcular (submuestras)
  dividendo = longitud    #dividendo para calcular numero de grupos
  grupos = 0              #numero de grupos
  
  #calculando el numero de grupos para grupos de al menos 4 elementos salvo el residual
  while (dividendo > 4) {
    dividendo = dividendo / 2
    potencia = potencia * 2
    grupos = grupos + 1
  }
  if(dividendo<4){
    grupos = grupos - 1
    potencia = potencia/2
  }
  #Comprobacion de parametros
  print(paste("Longitud, numero de filas: ",longitud))
  print(paste("Maxima potencia de 2 a calcular: ",potencia))
  print(paste("Numero de grupos: ",grupos))
  print(paste("Lo que queda de dividendo: ",dividendo))
  
  
  #matriz de indices (longitud x grupos)
  matriz_indices <- matrix(rep(0,longitud*grupos), ncol=grupos)
  #matriz de medias (longitud x grupos)
  matriz_medias <- matrix(rep(0,longitud*grupos), ncol=grupos)
  #matriz de desviaciones (longitud x grupos)
  matriz_desviaciones <- matrix(rep(0,longitud*grupos), ncol=grupos)
  #matriz de desviaciones acumuladas (longitud x grupos)
  matriz_desviaciones_acu <- matrix(rep(0,longitud*grupos), ncol=grupos)
  #matriz de rangos de las submuestras (longitud x grupos)
  matriz_rangos <- matrix(rep(0,longitud*grupos), ncol=grupos)
  #matriz de desviaciones típicas de las submuestras (longitud x grupos)
  matriz_std <- matrix(rep(0,longitud*grupos), ncol=grupos)
  #matriz de rango_reescalado de las submuestras (longitud x grupos)
  matriz_rrs <- matrix(rep(0,longitud*grupos), ncol=grupos)
  #vector media RS
  vector_rs <- matrix(rep(0,grupos))
  #vector numero observaciones
  vector_n <- matrix(rep(0,grupos))
  #vector media RS
  vector_log_rs <- matrix(rep(0,grupos))
  #vector numero observaciones
  vector_log_n <- matrix(rep(0,grupos))
  
  #Calculo de la matriz de indices
  #Se recorren todas las filas
  for(i in 1:longitud) {
    #para cada columna
    for(j in 1:grupos) {
      #Se rellena la matriz con la division entera del indice entre la potencia
      #de 2 correspondiente a la columna
      matriz_indices[i,j] <- indice[i]%/%(longitud%/%2^j)
    }
  }
 
  #Calculo de la matriz de medias
  #Para cada columna
  for (j in 1:grupos){ 
    #Un acumulador se inicializa con el primer valor del vector de diferencias
    acumulador <- v2[1]
    #Se inicializa un contador a 1
    contador <- 1
    #Indice para la fila de la matriz de medias media
    mmi=1
    #Vector para la media
    mm <- matrix(1:max(matriz_indices[,j]))
    
    #proceso, recorremos los vectores desde 2 hasta longitud total
    for(i in 2:longitud) {
      #Comprobamos si ha cambiado el indice de columna
      if(matriz_indices[i,j]!=matriz_indices[i-1,j]){
        mm[mmi]= acumulador / contador  #calculamos la media y la almacenamos 
                                        #en el vector de medias
        mmi=mmi+1                       #incrementamos el indice de medias
        acumulador <- v2[i]             #iniciamos el acumulador
        contador <- 1                   #iniciamos el contador
      } else {
        acumulador <- acumulador + v2[i]  #incrementamos el acumulador para la media
        contador <- contador + 1          #incrementamos el contador
      }
    }
    mm[mmi] = acumulador / contador
    
    #Calculo de la matriz de medias
    for(i in 1:longitud){
      matriz_medias[i,j] <- mm[matriz_indices[i,j]+1]
    }
    
  }

  #Calculo de matriz de desviaciones
  for(i in 1:longitud){
    for(j in 1:grupos){
      matriz_desviaciones[i,j]=v2[i]-matriz_medias[i,j]
    }
  }
  
  #Calculo de matriz de desviaciones acumuladas
  #inicializar primera fila
  matriz_desviaciones_acu[1,] = matriz_desviaciones[1,]
  for(j in 1:grupos) {
    for (i in 2:longitud) {
      if(matriz_indices[i,j]!=matriz_indices[i-1,j]){
        matriz_desviaciones_acu[i,j] = matriz_desviaciones[i,j]
      } else {
        matriz_desviaciones_acu[i,j] = matriz_desviaciones_acu[i-1,j]+matriz_desviaciones[i,j]
      }
    }
  }
  
  #Calculo de matriz de rangos de las submuestras
  for(j in 1:grupos) {
      for(i in 1:longitud) {
        matriz_rangos[i,j] = max(subset(matriz_desviaciones_acu[,j],matriz_indices[,j]==matriz_indices[i,j]))-min(subset(matriz_desviaciones_acu[,j],matriz_indices[,j]==matriz_indices[i,j]))
    }
  }
  #Calculo de matriz de desviaciones típicas de las submuestras
  for(j in 1:grupos) {
    for(i in 1:longitud) {
      matriz_std[i,j] = sd(subset(v2,matriz_indices[,j]==matriz_indices[i,j]))
    }
  }
  
  #Calculo de matriz de rango reescalado de las submuestras
  for(j in 1:grupos) {
    for(i in 1:longitud) {
      matriz_rrs[i,j] = matriz_rangos[i,j]/matriz_std[i,j]
    }
  }
  
  #Calculo de Vectores de regresión
  for(i in 1:grupos) {
      vector_rs[i] = mean(matriz_rrs[,i])
      vector_log_rs[i] = log(vector_rs[i])
      vector_n[i] = longitud/(2^i)
      vector_log_n[i] = log(vector_n[i])
  }
  
  #Calculo de la regresion
  plot(vector_log_n,vector_log_rs)
  regresion <- lm(vector_log_rs~vector_log_n)
  return(regresion$coefficients[2])
  
}

hurst_rs(close)
hurst_rs(BTC_USD$Close)
