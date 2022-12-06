options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)

data("iris")
str(iris)

pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)

set.seed(123)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- iris[ind==1,]
testing <- iris[ind==2,]

linear <- lda(Species~., training)
linear

p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$Species)
#Esses histogramas são baseados em ld1. Não há sobreposições entre a primeira e a segunda e a primeira e a terceira espécies. Mas alguma sobreposição observada entre a segunda e a terceira espécie.

ldahist(data = p$x[,2], g = training$Species)
#há sobreposição.Não é bom.

#Com base nas setas, a largura da sépala e o comprimento da sépala explicaram mais para a setosa, a largura da pétala e o comprimento da pétala explicaram mais para versicolor e virginica.
ggord(linear, training$Species, ylim = c(-10, 10))


p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Species)
tab

#Fazer para teste
