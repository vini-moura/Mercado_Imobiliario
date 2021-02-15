rm(list = ls(all=T))

library('ggplot2')
library('dplyr')
library('corrgram')
library('corrplot')
library('readxl')

setwd('~/Documentos/Projetos/Mercado Imobiliário/')
dados <- read.csv('casas.csv', sep=';', dec = ',')
str(dados)
View(dados)

corrgram(dados)
num <- sapply(dados, is.numeric)
correl <- cor(dados[,num])
View(correl)

casas <- subset(dados, dados$Tipo != 2 & dados$preco < 3000 & dados$A.const != 0)
str(casas)
ggplot(casas, aes(x=as.factor(Bairro), y=preco)) + geom_boxplot(ymin = 100,outlier.stroke = T) 
ggplot(casas, aes(x=A.total, y= preco)) + geom_point() + geom_smooth(se = T)
ggplot(casas, aes(x= A.const, y= preco)) + geom_point() + geom_smooth()
ggplot(casas, aes(x= A.total, y=preco)) + geom_boxplot()
ggplot(casas, aes(x= piscina, y=preco)) + geom_boxplot()
ggplot(casas, aes(x= as.factor(piscina), y=preco)) + geom_point()

casas2 <- casas[,c(-1,-5)]
casas2$Bairro <- as.factor(casas2$Bairro)
#modelo1 retirado por baixo R²
#modelo1 <- lm(data = casas2, preco ~ .)
modelo2 <- lm(data = casas2, preco ~ Bairro + A.total + A.const)
modelo3 <- lm(data = casas2, preco ~ Bairro + A.total + A.const + Sala.estar + Sala.jantar + Suite + Quarto + A.e.q + WC.social)

modelo2
modelo3

summary(modelo2)
summary(modelo3)

