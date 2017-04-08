#(1) Pacotes
install.packages("devtools")
install.packages("arules")
install.packages("ggplot2")
install.packages("caret")
install.packages("iterators")
install.packages("geosphere")

library(caret)
library(devtools)
library(ggbiplot)
library(arules)
library(ggplot2)
library(iterators)
library(geosphere)

#https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
#install_github("vqv/ggbiplot")

#(2) Funcoes
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#(3) Dataset
# load
baseSolo <- read.csv("C:\\Users\\unicesumar\\Downloads\\baseSolo.csv")

# analise
par(mfrow=c(4,5), mar=c(2,5,2,1), las=1, bty="n")
boxplot(baseSolo$Calcio, main="Calcio", ylab="Valores")
boxplot(baseSolo$Magnesio, main="Magnesio", ylab="Valores")
boxplot(baseSolo$Magnesio, main="Aluminio", ylab="Valores")
boxplot(baseSolo$Potassio, main="Potassio", ylab="Valores")
boxplot(baseSolo$Fosforo, main="Fosforo", ylab="Valores")
boxplot(baseSolo$Enxofre, main="Enxofre", ylab="Valores")
boxplot(baseSolo$MO, main="MO", ylab="Valores")
boxplot(baseSolo$Zinco, main="Zinco", ylab="Valores")
boxplot(baseSolo$Boro, main="Boro", ylab="Valores")
boxplot(baseSolo$Cobre, main="Cobre", ylab="Valores")
boxplot(baseSolo$Ferro, main="Ferro", ylab="Valores")
boxplot(baseSolo$Manganes, main="Manganes", ylab="Valores")
boxplot(baseSolo$MateriaOrganica, main="MateriaOrganica", ylab="Valores")
boxplot(baseSolo$CTC, main="CTC", ylab="Valores")
boxplot(baseSolo$SaturacaoPorBases, main="SaturacaoPorBases", ylab="Valores")
boxplot(baseSolo$CalcioSaturacaoDeBases, main="CalcioSaturacaoDeBases", ylab="Valores")
boxplot(baseSolo$MagnesioSaturacaoDebases, main="MagnesioSaturacaoDebases", ylab="Valores")
boxplot(baseSolo$PotassioSaturacaoDeBases, main="PotassioSaturacaoDeBases", ylab="Valores")
boxplot(baseSolo$pH, main="pH", ylab="Valores")
boxplot(baseSolo$TeorDeArgila, main="TeorDeArgila", ylab="Valores")

#limpeza

#Remove colunas com pouca variancia na baseSolo
baseSolo.nzv <- nearZeroVar(baseSolo, saveMetrics = TRUE)
baseSolo <- baseSolo[c(rownames(baseSolo.nzv[baseSolo.nzv$nzv == FALSE, ])) ]
rm(baseSolo.nzv)


#Remove coluna WGS84_ALT
baseSolo <- baseSolo[, !colnames(baseSolo) %in% c("WGS84_ALT") ]

# load
baseColheita <- read.csv("C:\\Users\\unicesumar\\Downloads\\baseColheita.csv")

# analise
par(mfrow=c(2,2), mar=c(2,5,2,1), las=1, bty="n")
boxplot(baseColheita$Rendimento.seco, main="Quartis", ylab="Valores")
hist(baseColheita$Rendimento.seco, main="Histograma", ylab="Frequencia")
d <- density(baseColheita$Rendimento.seco) # returns the density data 
plot(d, main="Densidade", ylab="Percentual") # plots the results
x <- baseColheita$Rendimento.seco 
h<-hist(x, main="Curva Normal", ylab="Frequencia") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

#limpeza

#Remove colunas sem importancia, mantendo apenas latitude, latitude, longitude e Rendimento.seco na baseColheita
baseColheita <- baseColheita[c("Latitude", "Longitude", "Rendimento.seco") ]

plot(baseColheita$Rendimento.seco)
baseColheita <- baseColheita[baseColheita$Rendimento.seco > 0 ,]

abline(h=mean(baseColheita$Rendimento.seco), col="green")
abline(h=mean(baseColheita$Rendimento.seco)*(1+(1/3)), col="blue")
abline(h=mean(baseColheita$Rendimento.seco)*(1-(1/3)), col="red")

summary(baseColheita$Rendimento.seco)

baseColheita <- baseColheita[baseColheita$Rendimento.seco < 8 | baseColheita$Rendimento.seco > 15 ,]

summary(baseColheita$Rendimento.seco)

#(4) Unir os datasets
radius <- 0
dataset <- cbind(baseSolo[sample(nrow(baseSolo), 0), ], baseColheita[sample(nrow(baseColheita), 0), ])

radiusOfTheEarth <- 6378137
minimumDistance <- 3

i <- 0
ibaseSolo <- iter(baseSolo, by = "row")
while(i < ibaseSolo$length) {
  solo = try(nextElem(ibaseSolo))
  i <- i + 1
  p1 <- c(solo$Longitude, solo$Latitude)
  print(i)
  #print(solo)
  
  j <- 0
  ibaseColheita <- iter(baseColheita, by = "row")
  while(j < ibaseColheita$length) {
    colheita = try(nextElem(ibaseColheita))
    j <- j + 1
    p2 <- c(colheita$Longitude, colheita$Latitude)
    #print(j)
    #print(colheita)
    
    d <- distHaversine(p1, p2, r=radiusOfTheEarth)
    radius <- rbind(radius,d)
    #print(d)
    
    if (d < minimumDistance) {
      #print("match")
      merged.data <- cbind(solo,colheita)
      dataset <- rbind(dataset,merged.data)
    }
    
  }
  
}

dataset <- read.csv("C:\\Users\\unicesumar\\Downloads\\dataset.csv")

#(5) PCA
dataset.new <- dataset[, -(17:18)]	#Retira Latitude e Longitude da baseSolo
dataset.pca <- prcomp(dataset.new[, 5:16], center = TRUE, scale. = TRUE) #Retira Latitude e Longitude da baseColheita para Calcular PCA

g <- ggbiplot(dataset.pca, groups = dataset$Produtividade, obs.scale = 1, var.scale = 1, circle = TRUE, ellipse = TRUE)
print(g)


#(6) MAPA
summary(dataset$Rendimento.seco)

dataset[, 'Produtividade'] <- ifelse(dataset$Rendimento.seco >= median(dataset$Rendimento.seco), 1, 0)
dataset$Produtividade[dataset$Rendimento.seco<9.3] <- 0 #baixa
dataset$Produtividade[dataset$Rendimento.seco>10.9] <- 1 #alta

alta <- dataset[which(dataset$Produtividade==1),]
baixa <- dataset[which(dataset$Produtividade==0),]

plot(baseSolo$Longitude, baseSolo$Latitude)
points(baseColheita$Longitude, baseColheita$Latitude, col = "green", cex = .8)
points(baseSolo$Longitude, baseSolo$Latitude, col = "black", cex = .8)
points(baixa$Longitude.1, baixa$Latitude.1, col = "red", cex = .6, bg = "red")
points(alta$Longitude.1, alta$Latitude.1, col = "blue", cex = .6, bg = "blue")


#(7) PADRAO
colunas <- c("Calcio","Magnesio","Al","Potassio","Fosforo","Enxofre","MO","CTC","SaturacaoPorBases","CalcioSaturacaoDeBases","MagnesioSaturacaoDebases","PotassioSaturacaoDeBases","pH")
baseAlta.new <- dataset[which(dataset$PontosAmostrais == 107), colunas]
baseAlta.new[, colunas] <- lapply(baseAlta.new[, colunas], factor)
baseAlta.tr <- apriori(
  baseAlta.new,
  parameter = list(support = 1, minlen = 13, maxlen = 13, target = "frequent")
)

inspect(baseAlta.tr)