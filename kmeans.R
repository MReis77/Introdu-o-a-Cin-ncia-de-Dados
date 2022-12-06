library(FactoMineR)
library(factoextra)
library(cluster)
library(readxl)

MundoCrime <- read_excel("C:/Users/Opencadd/Desktop/World Crime Index.xls")

View(MundoCrime)

MundoCrime$`Crime Index` <- as.numeric(MundoCrime$`Crime Index`)
MundoCrime$`Safety Index`<- as.numeric(MundoCrime$`Safety Index`)

boxplot(MundoCrime$`Crime Index`)

#Tirar a coluna City e colocar ela como rotulo
dados <- MundoCrime [, -0]
row.names(dados) <- MundoCrime[,2]

dados <- scale(dados)

#Definir quantidade otima de cluster
fviz_nbclust(dados, kmeans, method = "gap_stat")

#Gerar o kmeans

dados_kmeans <- kmeans(dados, 4)

#View gráfico

fviz_cluster(dados_kmeans, data = dados)


data("USArrests")    # Loading the data set
df <- scale(USArrests) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)
