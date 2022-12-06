#Instalando pacotes

install.packages('ggplot2')
install.packages("dplyr")
install.packages("devtools")
install.packages("caret")
install.packages("Factoshiny")

#Load nas bibliotecas

library(ggplot2)
library(dplyr)
library(devtools)
library(scatterplot3d)
library(readxl)
library(caret)
library(fBasics)
#Importando as databases

Montreal <- read_excel("C:/Users/Opencadd/Desktop/Project/Montreal Crime Data.xls")
Mundo <- read_excel("C:/Users/Opencadd/Desktop/Project/World Crime Index.xls")

#Análise Explicita============================================================================================================

View(Montreal)
View(Mundo)

#Filtrando Montreal

Montreal [1] #Trazendo a primeira coluna
Montreal [1,] #Traz os valores da primeira linha

Montreal [-4] #Excluindo uma coluna da análise

Montreal$year == 2021 #Retorna onde é TRUE or FALSE na coluna selecionada

Montreal[Montreal$year == 2021,] #Mostra os dados de onde apenas tem o filtro 2021

Montreal <- Montreal[c(-8, -9, -10)] #Modificando a base de dados

#Filtrando Mundo

Mundo [1,1] #Trazendo a primeira linha e a primeira coluna

Mundo [-1] #Exluindo a primeira coluna da análise

Mundo$`Crime Index` < 50.0

#Análise Exploratória=========================================================================================================

names(Montreal)
names(Mundo)

#Análise Montreal

summary(Montreal)
summary(Mundo)

Montreal$category <- as.factor(Montreal$category)
Montreal$neighbourhood <- as.factor(Montreal$neighbourhood)
Montreal$year <- as.numeric(Montreal$year)

plot(Montreal$category, Montreal$year)

plot(
  Montreal$category, #cordenada X
  Montreal$neighbourhood, #cordenada Y
  main = "Campo Experimental Criminalidade",
  xlab = "Crime", #legenda eixo x
  ylab = "Vizinhança", #leganda eixo y
  asp = 1, #aspecto gráfico (mesma escala para eixos)
  pch =16, #tipo de caracter a ser exibido
  cex = 3, #tamanho caracter
  col = "red"
)

basicStats(Montreal$year) #Comportamento dos dados

histPlot(as.timeSeries(Montreal$year))

boxPlot(Montreal$year)

ggplot(Montreal, aes(x=category, y=neighbourhood,))+
  geom_point()

#análise Mundo

Mundo$`Crime Index` <- as.numeric(Mundo$`Crime Index`)
Mundo$`Safety Index` <- as.numeric(Mundo$`Safety Index`)

plot(Mundo$`Crime Index`, Mundo$`Safety Index`) #Simples plotagem

plot(
  Mundo$`Crime Index`, #cordenada X
  Mundo$`Safety Index`, #cordenada Y
  main = "Campo Experimental Criminalidade",
  xlab = "Crime", #legenda eixo x
  ylab = "Safety", #leganda eixo y
  asp = 1, #aspecto gráfico (mesma escala para eixos)
  pch =16, #tipo de caracter a ser exibido
  cex = 3, #tamanho caracter
  col = "red"
)

basicStats(Mundo$`Crime Index`) #Comportamento dos dados

histPlot(as.timeSeries(Mundo$`Crime Index`))

boxPlot(Mundo$`Crime Index`)

ggplot(Mundo, aes(x=City, y=`Safety Index`,))+
  geom_point()

#Análise Implicita============================================================================================================

View(Montreal)
View(Mundo)

str(Montreal)    
str (Mundo)

#Mudar tipo de variáveis

summary(Montreal$year)

Montreal$year[Montreal$year==2021] <- "Crime Recente"
Montreal$year <- as.factor(Montreal$year)

summary(Montreal$year)
str((Montreal$year))

summary(Montreal$neighbourhood)
Montreal$neighbourhood <- as.factor(Montreal$neighbourhood)
summary(Montreal$neighbourhood)

#Machine Learning

library(Factoshiny)

res_pca <- PCAshiny(Mundo)

correlacao <- cor(Mundo[,-2]) #Tirando a váriavel categórica

corrplot::corrplot(correlacao, type = "upper")

#Para a técnica tanto do shiny quanto do machine learning, foi utilizado a técnica PCA
# Uma das técnicas mais utilizadas na redução de dimensionalidade é um método estatístico designado por Principal Component Analysis (PCA).
#O PCA é caracterizado por identificar as dimensões ao longo das quais os dados se encontram mais dispersos. Desta forma, 
#conseguimos identificar as dimensões que melhor diferenciam o conjunto de dados em análise, ou seja, os seus componentes principais.

#Usando esta técnica, é possível realçar as semelhanças e diferenças neles existentes através da identificação de padrões. 
#A sua identificação em dados caraterizados por grandes dimensões é difícil, uma vez que a sua representação gráfica não é viável, 
#logo uma análise visual aos dados não é possível. Quando identificados os padrões no conjunto, o número de dimensões a analisar pode
#ser reduzido sem que haja uma perda significativa de informação, pois o foco recai sobre a análise das dimensões principais que
#caracterizam o conjunto de dados.


