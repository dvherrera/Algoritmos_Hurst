library(RCurl)
ligaCsv=getURL("http://www.dma.ulpgc.es/profesores/personal/stat/datos/liga2013-14.csv")
liga1314=read.csv2(textConnection(ligaCsv),encoding="UTF-8",stringsAsFactors=FALSE)
head(liga1314)
