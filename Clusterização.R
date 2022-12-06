#Clusterização simples=========================================================================================

data ("mtcars") #Chamando uma base de dados pré-existente no RStudio


View(mtcars)


?mtcars #Traz as informações de um dataset ou até de comandos do R


mpg_cyl <- mtcars[,c(1,2)] #Criando um novo dataset apenas com a coluna 1 e 2


grupo <- hclust(dist(mpg_cyl))

plot (grupo) #Plota os carros mais economicos, mais ou menos e beberrões

mpg_cyl<- scale(mpg_cyl)
 
#Definir quantidade otima de cluster
fviz_nbclust(mpg_cyl, kmeans, method = "gap_stat")

#Gerar o kmeans

dados_kmeans <- kmeans(mpg_cyl, 4)

#View gráfico

fviz_cluster(dados_kmeans, data = mpg_cyl)
