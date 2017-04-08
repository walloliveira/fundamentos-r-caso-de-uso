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
baseSolo <- read.csv("baseSolo.csv")

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

baseColheita <- read.csv("baseColheita.csv")

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




