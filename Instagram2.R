#### Modesto Escobar, Carlos Prieto & David
# Thu Oct 15 17:25:47 2020 ------------------------------

library(tidyverse)
library(netCoin)
library(tweetCoin)
library(readxl)


load("TrumpNetwork-M.RData")


C <- netCoin(nodesf,adjmat,name="ID",image="image", repulsion=15, zoom=0.2,
             main="Trump's Instagram (Similar people)")


D <- read_excel("Donald Trump Instagram Sept-2020 Clean.xlsx")
L <- dichotomize(D, "labels_txt", sep=", ", add=FALSE, sort=TRUE)
I <- as.data.frame(t(as.matrix(L)))
ta <- sort(apply(I,1,sum), decreasing = TRUE)
j <- I[ta>1,]
k <- j
j <- j[!row.names(j) %in% c("Photo caption", "Photogtaphy", "Font","Text","Line"),]
H <- allNet(j, frequency=TRUE)
I <- allNet(k, frequency=TRUE)



E <- read_excel("Donald Trump Instagram Sept-2020 Clean.xlsx", sheet="Trabajo")

E$date <- as.POSIXct(E$date)
E$id <- E$id+1
E$labels_txt <- gsub(", ","|",E$labels_txt)
E$name <- paste0("V",E$id)
E$image <- paste0("./images/",E$id,".jpg")
E$url   <- urltoText(E$url)
fields <- cc("name, date, comment_count, like_count, labels_txt, adult, violence, racy, spoof, medical, image, url")
nodesf$name <- paste0("V",nodesf$ID)
field  <- cc("name, num, young, senior, black, white, hispanic, asian, female")

M  <- merge(E[,fields], nodesf[, field], by="name", all.x=TRUE)

H$nodes <- merge(H$nodes[,1:2], M, by="name", all.x=TRUE)
H$links <- H$links[H$links$Haberman>=sqrt(nrow(j)),]
g <- netCoin(H, degreeFilter = 1, image="image", ntext="url", zoom=0.6, repulsion=5, main= "Trump's Instagram (Equal labels without Font, Text, Line, Photo)")


I$nodes <- merge(I$nodes[,1:2], M, by="name", all.x=TRUE)
I$links <- I$links[I$links$Haberman>=sqrt(nrow(k)),]
G <- netCoin(I, degreeFilter = 1, image="image", ntext="url", zoom=0.6, repulsion=5, main= "Trump's Instagram (Equal labels)")


dico <- function(vector, umbral=.5, top=1, bottom=0, na=0) {
  vector[is.na(vector)] <- na
  vector <- ifelse(vector>=umbral, top, bottom)
}


S <- H$nodes
S[,field[-(1:2)]] <- sapply(S[,field[-(1:2)]], dico, top="YES", bottom="NO", na="NO")
others <- cc("adult, violence, racy, spoof, medical")
W<- surScat(S,variables=c(field, others, "date", "comment_count", "like_count", "image"), active=c(field[-1], others), nclusters = 2:6,
            image="image", main="Trump's Instagram (Scattergram)")

multigraphCreate("Similar people"=C, "Equal labels (max)"=G, "Equal labels (min)"=g,  "Scattergram"=W, dir="./html")



