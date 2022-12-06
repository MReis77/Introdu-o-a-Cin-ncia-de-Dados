library(FactoMineR)
library(factoextra)
library(cluster)
library(readxl)

data ("mtcars")

head(mtcars, 3)

dados_car <- scale(mtcars)

dados_kmeans_car <- kmeans(mtcars, 4)

fviz_cluster(dados_kmeans_car, data = mtcars)
