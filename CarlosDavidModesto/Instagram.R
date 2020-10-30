
library(readxl)
library(netCoin)

load("~/Trabajo/Proyectos/Coincidencias/Instagram/imageAnalysisResult.RData")
D <- read_excel("Donald Trump Instagram Sept-2020 Clean.xlsx")
E <- read_excel("Donald Trump Instagram Sept-2020 Clean.xlsx", sheet="Trabajo")

L <- dichotomize(D, "labels_txt", sep=", ", add=FALSE, sort=TRUE)
I <- as.data.frame(t(as.matrix(L)))
dim(I)
set.seed(2020)
i <- I[,sample(1:ncol(I),500)]


ta <- sort(apply(I,1,sum), decreasing = TRUE)
j <- I[ta>1,]


H <- allNet(j)
H$links <- H$links[H$links$Haberman>29.068875,]
netCoin(H, degreeFilter = 1)
plot(H)
