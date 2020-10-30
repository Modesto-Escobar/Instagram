library(readxl)
library(dplyr)
library(tidyverse)
library(netCoin)
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(SnowballC)
library(igraph)


datos <- read_excel("Donald Trump Instagram Sept-2020 Clean.xlsx")


corpus <- Corpus(VectorSource(datos$labels_txt)) # formato de texto

# lleva a minúsculas
d  <- tm_map(corpus, tolower)

# quita espacios en blanco
d  <- tm_map(d, stripWhitespace)

# quita la puntuación
d <- tm_map(d, removePunctuation)

# quita los números
d <- tm_map(d, removeNumbers)

# remueve palabras vacías genericas
d <- tm_map(d, removeWords, stopwords("english"))

# crea matriz de términos
tdm <- TermDocumentMatrix(d)

frecuentes<-findFreqTerms(tdm, lowfreq=20)

m <- as.matrix(tdm) #lo vuelve una matriz
v <- sort(rowSums(m),decreasing=TRUE) #lo ordena y suma
df <- data.frame(word = names(v),freq=v) # lo nombra y le da formato de data.frame
dfnube <-df
df$from <-""
names(df) = c("to", "frecuencia", "from")
df = df %>% select(from, to, frecuencia)
df0 <- df[df$frecuencia>200,]

net <- graph_from_data_frame(df0, directed=F)
df1 <- as.data.frame(as.numeric("2"))
names(df1) = c("tam")
df2 <- as.data.frame(as.numeric(df$frecuencia))
names(df2) = c("tam")
frecuencias <- unlist(rbind(df1 ,df2))
V(net)$tam <- frecuencias 

#convertimos el objeto igraph a objeto netcoin
g<-fromIgraph(net)

#creamos las opciones
g2<-netCoin(g$nodes,g$links,label="name",size = "tam", labelSize = "tam",layout = "ka")

plot(g2)

### HISTOGRAMA FRECUENCIA DE PALABRAS
barplot(dfnube[1:10,]$freq, las = 2, names.arg = dfnube[1:10,]$word,
        col ="lightblue", main ="PALABRAS MÁS FRECUENTES", ylab = "Frecuencia de palabras")

### Tres versiones de de nube de palabras
wordcloud(words = dfnube$word, freq = dfnube$freq, min.freq = 6,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(dfnube, size=1.2)

wordcloud2(dfnube, size = 0.5, ellipticity = 0.1)



