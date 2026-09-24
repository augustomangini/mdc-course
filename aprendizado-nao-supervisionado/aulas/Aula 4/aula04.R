##################################################################
# Mineracao de Dados Complexos -- MDC 
# Aprendizado de Máquina Não Supervisionado
# Prof. Helio Pedrini
# Codigos da Aula 4 - Tecnicas de Agrupamento
##################################################################

# Carrega a biblioteca
library(hopkins)

# Gera dados sintéticos (três grupos gaussianos)
set.seed(123)

x1 <- matrix(rnorm(300, mean=0, sd=0.3), ncol=2)
x2 <- matrix(rnorm(300, mean=3, sd=0.3), ncol=2)
x3 <- matrix(rnorm(300, mean=6, sd=0.3), ncol=2)

X <- rbind(x1, x2, x3)
par(mar=c(4, 4, 2, 2))
# Visualiza os dados
plot(X, col="blue", pch=16, main="")

# Calcula a estatı́stica de Hopking
hopkins(X)

# Gera dados sintéticos (uniformes)
set.seed(123)
X <- matrix(runif(600), ncol=2)

# Visualiza os dados
plot(X, col="blue", pch=16, main="")

# Calcula a estatı́stica de Hopking
hopkins(X)

# Carrega o conjunto de dados
library(datasets)
data(iris)

# Apresenta um resumo da base:
summary(iris)

# Calcula a matriz de distancias:
d <- dist(iris[, 3:4], method="euclidean")

# Visualiza a matriz de distancias com a funcao heatmap
heatmap(as.matrix(d), symm=TRUE)

# Aplica o algoritmo de agrupamento hierarquico
clusters <- hclust(d, method="complete")

# Apresenta o dendrograma resultante:
plot(clusters)

# Corta o dendrograma no numero k de grupos e retorna o 
# agrupamento dos dados
clusters_cut <- cutree(clusters, k=3)

# Mostra os resultados
table(clusters_cut, iris$Species)

# Visualiza os agrupamentos
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, 
                 color=Species)) + 
  geom_point(alpha=0.4, size=3.5) + 
  geom_point(col=clusters_cut) + 
  scale_color_manual(values=c('black', 'red', 'green'))


# Outra estrategia de proximidade pode ser avaliada:
clusters <- hclust(d, method='average')

# Apresenta o dendrograma resultante
plot(clusters)

# Atribuindo-se o numero de grupos igual a 3
clusters_cut <- cutree(clusters, k=3)

# Mostra os resultados
table(clusters_cut, iris$Species)

# Visualiza os agrupamentos
ggplot(iris, aes(Petal.Length, Petal.Width, 
                 color=Species)) + 
  geom_point(alpha=0.4, size=3.5) + 
  geom_point(col=clusters_cut) + 
  scale_color_manual(values=c('black', 'red', 'green'))

# Aplica o algoritmo K-Means com k=3
# Resultados podem variar devida a aleatoriedade
cl1 <- kmeans(iris[,1:4], 3)
cl1

# Retorna o numero de pontos em cada grupo
cl1$size

# Retorna o numero de centroides 
cl1$centers

# Retorna a indicacao do grupo de cada dado
cl1$cluster

# Retorna a matriz de resultados
table(cl1$cluster, iris$Species)

# Diferente inicializacao
cl2 <- kmeans(iris[,1:4], 3, nstart=20)
cl2

# Calcula a matriz de dissimilaridades
library(cluster)
dissE <- daisy(iris[,1:4])

# Calcula as silhuetas
sk <- silhouette(cl1$cl, dissE)

# Apresenta as silhuetas
plot(sk)

# Outra forma de calcular e visualizar as silhuetas
library(factoextra)
fviz_silhouette(sk)

# Determina o numero otimo de grupos
library(NbClust)

# remove coluna com rótulos das classes e escala os dados
iris.scaled <- scale(iris[, -5])

# Gera os graficos referentes as medidas Hubert index e D index
# São gerados 4 gráficos
nb <- NbClust(iris.scaled, distance="euclidean", 
              min.nc=2, max.nc=10, method="complete", 
              index="all")

# Valor de cl$totss
cl1$totss

# Valor de cl1$withinss
cl1$withinss

# Valor de cl1$tot.withinss
cl1$tot.withinss

# Valor de cl1$betweenss
cl1$betweenss

# Pode-se ainda calcular um conjunto variado de medidas 
# estatísticas para o algoritmo K-Means
library(fpc)

# Compute pairwise-distance matrices
dd <- dist(iris.scaled, method="euclidean")

# Statistics for k-means clustering
km_stats <- cluster.stats(dd, cl1$cluster)
km_stats

# Aplica a tecnica K-Means
km.res <- eclust(iris.scaled, "kmeans", k=3,
                 nstart=25, graph=FALSE)

# Mostra o numero do grupo de cada amostra
km.res$cluster

# Visualiza os grupos (versão depreciada)
# fviz_cluster(km.res, geom="point", ellipse.type="norm")

# Visualiza os grupos
fviz_cluster(km.res, geom="point", ellipse.type="norm")

# Aplica a tecnica PAM
pam.res <- eclust(iris.scaled, "pam", k=3, graph=FALSE)

# Mostra o numero do grupo de cada amostra
pam.res$cluster

# Visualiza os grupos
fviz_cluster(pam.res, geom ="point", ellipse.type ="norm")


# Carrega pacotes e base
library(cluster)
library(factoextra)
library(NbClust)

data(USArrests)

# Remove dados faltantes e normaliza dados
df <- USArrests
df <- na.omit(df)
df <- scale(df)

# Apresenta os resultados
head(df, n=3)

# Aplica a tecnica K-Means com dois grupos
set.seed(123)
k2 <- kmeans(df, centers=2, nstart=25)
k2

# Adiciona classificacao aos dados originais
dd <- cbind(USArrests, cluster=k2$cluster)
head(dd)

# Mostra o tamanho dos grupos
k2$size


# Mostra o numero dos grupos para cada amostra
k2$cluster
head(k2$cluster, 4)


# Mostra os centroides dos grupos
k2$centers

# Visualiza os resultados
fviz_cluster(k2, data=df)

# Testa diferentes numeros de grupos
k3 <- kmeans(df, centers=3, nstart=25)
k4 <- kmeans(df, centers=4, nstart=25)
k5 <- kmeans(df, centers=5, nstart=25)

# Mostra os agrupamentos
p1 <- fviz_cluster(k2, geom="point", data=df) + ggtitle("k=2")
p2 <- fviz_cluster(k3, geom="point", data=df) + ggtitle("k=3")
p3 <- fviz_cluster(k4, geom="point", data=df) + ggtitle("k=4")
p4 <- fviz_cluster(k5, geom="point", data=df) + ggtitle("k=5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow=2)

# Determina o numero de grupos
set.seed(123)
fviz_nbclust(df, kmeans, method="wss")

# Determina o numero de grupos (medida de silhueta)
fviz_nbclust(df, kmeans, method="silhouette")

# Extrai resultados, assumindo um numero de grupos igual a 4
set.seed(123)
final <- kmeans(df, centers=4, nstart=25)
print(final)

# Visualiza os resultados
fviz_cluster(final, data = df)

# # Carregando base wine com arquivo offline
# # baixado do Moodle:
# wine <- read.table(file.choose(), sep=",")

# Carrega base de dados com arquivo online
wine <- 
  read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
             sep=",")

# Normaliza os dados
wine.stand <- scale(wine[-1])

# Aplica a tecnica de agrupamentos K-Means
km <- kmeans(wine.stand, 3)

# Apresenta os centroides do grupos
km$centers

# Apresenta os grupos
km$cluster

# Apresenta os tamanhos dos grupos
km$size


# Visualiza os grupos
library(cluster)
clusplot(wine.stand, km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Mostra o desempenho dos agrupamentos
table(wine[,1], km$cluster)

# Aplica a tecnica de agrupamento hierarquico, tendo como entrada uma matriz de distancias
d <- dist(wine.stand, method="euclidean")
h <- hclust(d, method="ward.D")

# Visualiza dendrograma
plot(h)

# Corta o dendrograma em 5 grupos e mostra o desempenho dos agrupamentos
groups <- cutree(h, k=3)
table(wine[,1], groups)

# Apresenta medidas de desempenho
km$betweenss

km$withinss

km$tot.withinss

km$totss

# Instala pacotes e carrega base
# install.packages("dbscan")
library(dbscan)
data(multishapes, package="factoextra")
dados <- multishapes[, 1:2]

# Executa algoritmo de agrupamentos DBSCAN
set.seed(123)
db <- dbscan::dbscan(dados, eps=0.15, minPts=5)

# Apresenta os resultados
print(db)

# Visualiza os grupos
# # Note que aqui usamos apenas a função plot pois os dados 
# # possuem apenas duas dimensões, para criar gráficos de 
# # dispersão para bases com mais dimensões use as funções
# # do pacote fviz.
plot(dados, col=db$cluster)
points(dados[db$cluster==0,], pch=3, col="grey")

# Outra forma de visualizar os agrupamentos
library("factoextra")

fviz_cluster(db, data=dados, stand=FALSE, 
             ellipse=FALSE, show.clust.cent=FALSE, 
             geom="point", palette="jco",
             ggtheme=theme_classic())

# Para determinar o valor otimo de Eps
dbscan::kNNdistplot(dados, k = 5)

# Caso a tecnica K-Means fosse utilizada
library(factoextra)
data("multishapes")
df <- multishapes[, 1:2]

set.seed(123)
km.res <- kmeans(df, 5, nstart=25)

# O resultado seria
fviz_cluster(km.res, df,  geom="point", 
             ellipse=FALSE, show.clust.cent=FALSE,
             palette="jco", ggtheme=theme_classic())

# Carrega pacotes e base de dados
library(dbscan)
data("DS3")

# Executa o algoritmo HDBSCAN
hd <- hdbscan(DS3, minPts=25)
print(hd)

# Visualiza os grupos
plot(DS3, col = hd$cluster + 1, pch = 24, cex = 0.25)

# Carrega pacotes e base de dados
library(dbscan)
data("moons")

# Executa o algoritmo HDBSCAN
hd<-hdbscan(moons, minPts = 5)
# Visualiza os grupos
plot(moons, col = hd$cluster + 1, pch=20)


# Carrega pacotes e base de dados
# install.packages("ppclust")
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)

data(iris)

# Separa atributos dos dados
x <- iris[,-5]
print(x)

# Aplica o algoritmo Fuzzy C-Means e apresenta grau de pertinencia
res.fcm <- fcm(x, centers=3)
as.data.frame(res.fcm$u)

# Mostra prototipos dos grupos finais
res.fcm$v

# Apresenta resumo dos agrupamentos
summary(res.fcm)

# Visualiza resultados dos agrupamentos por meio de 
# pares de atributos com a funcao plotcluster
plotcluster(res.fcm, cp=1, trans=TRUE)

# Visualiza resultados dos agrupamentos por meio da funcao fviz cluster
res.fcm2 <- ppclust2(res.fcm, "kmeans")
factoextra::fviz_cluster(res.fcm2, data = x, 
                         ellipse.type = "convex",
                         palette = "jco",
                         repel = TRUE)


# Calcula e apresenta medidas de avaliacao
res.fcm4 <- ppclust2(res.fcm, "fclust")
idxsf <- SIL.F(res.fcm4$Xca, res.fcm4$U, alpha=1)
idxpe <- PE(res.fcm4$U)

cat("Fuzzy Silhouette Index: ", idxsf)

cat("Partition Entropy: ", idxpe)


# Carrega pacote e base de dados
library(mlbench)

dados <- mlbench.spirals(100, 1, 0.001)

# Visualiza o conjunto de dados
plot(dados$x, pch=15, col="black")
title('conjunto de dados')

# Aplica a tecnica de agrupamentos K-Means
km <- kmeans(dados$x, centers=2)

plot(dados$x, pch=15, col=km$cluster)
title('agrupamento K-means')

# Aplica a tecnica de agrupamentos espectral
library(kernlab)
sc <- specc(dados$x, centers=2)

plot(dados$x, col=sc)
title('agrupamento espectral')

  
# Instala pacotes e carrega base:
# install.packages("apcluster")
library(apcluster)
data(iris)

# Executa algoritmo de propagacao de afinidade:
ap_iris1 <- apcluster(negDistMat(r=2), iris, q=0.5)

# Apresenta resultados (6 grupos foram identificados):
ap_iris1

# O resultado pode ser apresentado na forma de graficos de dispersao:
plot(ap_iris1, iris)

# Matriz de similaridade correspondente:
heatmap(ap_iris1)

# Executa algoritmo usando o minimo das similaridades:
ap_iris2 <- apcluster(negDistMat(r=2), iris, q=0)
ap_iris2

# Resultado na forma de graficos de dispersao:
plot(ap_iris2, iris)

# Matriz de similaridade correspondente:
heatmap(ap_iris2)


# Códigos auxiliares para os exercicios propostos
# Exercicio 1
iris.pca1 <- prcomp(iris[,1:4], scale.=TRUE)
cl3 <- kmeans(iris.pca1$x[,1:1], 3, nstart=20)

# Exercicio 2
install.packages("PCAmixdata") 
library(PCAmixdata)
data(protein)


# Algoritmo Mean Shift no Pacote R
# Carrega biblioteca
library(MASS)

# Gera conjunto de pontos
set.seed(42)  # para reprodutibilidade

grupo1 <- mvrnorm(n=1000, mu=c(5, 5),
                  Sigma=matrix(c(4, 0, 0, 4), nrow=2))

grupo2 <- mvrnorm(n=1000, mu=c(15, 15),
                  Sigma=matrix(c(9, 0, 0, 9), nrow=2))

grupo3 <- mvrnorm(n=1000, mu=c(17, 3),
                  Sigma=matrix(c(1, 0, 0, 1), nrow=2))

grupo4 <- mvrnorm(n=1000, mu=c(2, 16),
                  Sigma=matrix(c(1, 0, 0, 1), nrow=2))

dados1 <- rbind(grupo1, grupo2, grupo3, grupo4)

# Visualiza o conjunto de dados
library(ggplot2)

ggplot(as.data.frame(dados1), aes(x=V1, y=V2)) +
geom_point(color="black", size=0.8) +
theme_minimal() +
coord_fixed(ratio=1) +
theme(plot.title=element_text(hjust=0.5, size=14),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border=element_rect(color="black", fill=NA, size=1))

# Executa o algoritmo Mean Shift
library(meanShiftR)

# estima largura de banda para cada dimens�o com base 
# no quantil do desvio padr�o
quantile <- 0.57
bandwidth <- apply(dados1, 2, sd) * quantile

# aplica algoritmo de agrupamento com kernel linear
resultado_mean_shift <- meanShift(dados1, 
                                  algorithm="LINEAR",
                                  bandwidth=bandwidth)

# Extrai grupos identificados:
grupos <- resultado_mean_shift$assignment

# Mostra o resultado:
plot(dados1, col=rainbow(length(unique(grupos)))[grupos],
     pch=19, main="", xlab="X", ylab="Y")


# Carrega biblioteca:
library(clusterSim)

# Gera conjunto de pontos:
circulos <- shapes.circles2(250) 
dados2 <- circulos$data 

# Visualiza o conjunto de dados:
library(ggplot2)

plot(dados2, col=rainbow(2)[circulos$clusters],
     pch=19, main="")

# Executa o algoritmo Mean Shift:
library(meanShiftR)

# estima largura de banda para cada dimensão com base 
# no quantil do desvio padrão
quantile <- 0.75
bandwidth <- apply(dados2, 2, sd) * quantile

# aplica algoritmo de agrupamento com kernel linear
resultado_mean_shift <- meanShift(dados2, 
                                  algorithm="LINEAR",
                                  bandwidth=bandwidth)

# Extrai grupos identificados:
grupos <- resultado_mean_shift$assignment

# Mostra o resultado:
plot(dados2, col=rainbow(length(unique(grupos)))[grupos],
     pch=19, main="", xlab="X", ylab="Y")

# Carrega as bibliotecas
library(tidyverse)
library(factoextra)
library(dplyr)
library(dbscan)
library(kernlab)
library(apcluster)

# Lê o conjunto de dados
base <- read.csv("https://www.ic.unicamp.br/~helio/datasets/Country-data.csv")

# Visualiza as primeiras linhas do conjunto de dados
head(base)

# Verifica se há dados faltantes
colSums(is.na(base))

# Separa atributos numéricos e nomes dos paı́ses
dados <- base[, -1]
paises <- base[, 1]

# Normaliza os dados
dados <- as.data.frame(scale(dados))

# Aplica a técnica de agrupamento hierárquico
clusters <- hclust(get_dist(dados, method="euclidean"), 
                   method="complete")

# Armazena os grupos como dendrograma
clusters_dend <- as.dendrogram(clusters)

# Mostra o dendrograma completo e adiciona retângulos ao redor dos grupos
plot(clusters_dend, main="")
rect.hclust(clusters, k=5, border=3:6) 

# Mostra os ramos acima do ponto de corte
plot(cut(clusters_dend, h=8)$upper, main="")

# Mostra um ramo especı́fico abaixo do corte
plot(cut(clusters_dend, h=8)$lower[[2]], main="")

# Corta o dendrograma em uma determinada altura
grupos <- cutree(clusters, k=5)

# Adiciona os rótulos dos agrupamentos ao conjunto de dados original
df <- base %>% mutate(cluster=grupos) %>%
  relocate(cluster, .after=country)

# Filtra paı́ses pertencentes a um grupo especı́fico
df %>% filter(cluster == 3)

# Mostra os grupos obtidos
fviz_cluster(list(data=dados, cluster=grupos), main="")

# Gráfico de GDP per capita versus expectativa de vida
ggplot(df, aes(x=gdpp, y=life_expec, col=as.factor(cluster), 
               label=country)) + geom_point() + geom_text() +
  ggtitle("") + xlab('GDP') + ylab('Expectativa de Vida') +
  scale_x_log10() 


# Gráfico de GDP per capita versus mortalidade infantil
ggplot(df, aes(x=gdpp, y=child_mort, col=as.factor(cluster), 
               label=country)) + geom_point() +  geom_text() +
  ggtitle("") + xlab('GDP') + ylab('Mortalidade Infantil') +
  scale_x_log10() + scale_y_log10()

# Aplica a técnica K-Means com um número arbitrário de grupos
k_means <- kmeans(dados, centers=3)

# Após obter os agrupamentos, adiciona a classificação ao conjunto original
dados_kmeans <- base %>% mutate(cluster=k_means$cluster)

# Gráfico do cotovelo baseado na soma dos quadrados intra-grupo para k = 1 até k = 8
tot_withinss <- sapply(1:8, function(k){
  km_mod <- kmeans(x=dados, centers=k)
  km_mod$tot.withinss
})

# Armazena os resultados em um data frame
tot_withinss_df <- data.frame(k=1:8, tot_withinss=tot_withinss)

# Mostra o gráfico do cotovelo
ggplot(tot_withinss_df, aes(x=k, y=tot_withinss)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = 1:8) +
  ggtitle("") + ylab("Soma dos Quadrados Intra-Grupo") + xlab("k")

# Utiliza silhueta para avaliar número de grupos
fviz_nbclust(dados, kmeans, method="silhouette")

# Aplica K-Means com diferentes números de grupos (3 a 6)
k_means_3 <- kmeans(dados, centers=3)
k_means_4 <- kmeans(dados, centers=4)
k_means_5 <- kmeans(dados, centers=5)
k_means_6 <- kmeans(dados, centers=6)

# Adiciona os rótulos dos agrupamentos ao conjunto de dados original
dados_kmeans_3 <- base %>% mutate(cluster=k_means_3$cluster) 
dados_kmeans_4 <- base %>% mutate(cluster=k_means_4$cluster) 
dados_kmeans_5 <- base %>% mutate(cluster=k_means_5$cluster) 
dados_kmeans_6 <- base %>% mutate(cluster=k_means_6$cluster)

# Mostra os grupos para k = 3
fviz_cluster(list(data=dados, cluster=k_means_3$cluster), main="")

# Mostra os grupos para k = 4
fviz_cluster(list(data=dados, cluster=k_means_4$cluster), main="")

# Mostra os grupos para k = 5
fviz_cluster(list(data=dados, cluster=k_means_5$cluster), main="")

# Mostra os grupos para k = 6
fviz_cluster(list(data=dados, cluster=k_means_6$cluster), main="")

# Gráfico de GDP per capita versus expectativa de vida
ggplot(dados_kmeans_4, aes(x=gdpp, y=life_expec, 
                           col=as.factor(cluster), label=country)) + 
  geom_point() + geom_text() +
  ggtitle("") + xlab("GDP") + ylab("Expectativa de Vida") +
  scale_x_log10() + scale_y_log10()

# Gráfico de GDP per capita versus mortalidade infantil
ggplot(dados_kmeans_4, aes(x=gdpp, y=child_mort, 
                           col=as.factor(cluster), label=country)) + 
  geom_point() + geom_text() +
  ggtitle("") + xlab("GDP") + ylab("Mortalidade Infantil") +
  scale_x_log10() + scale_y_log10()

# Aplica a técnica HDBSCAN
hdb <- hdbscan(dados, minPts=5)

# Rótulos dos grupos (0 indica outliers)
hdb$cluster

# Adiciona os rótulos dos agrupamentos ao conjunto de dados original
df_hdb <- base %>% mutate(cluster=hdb$cluster)

# Visualiza quantos pontos em cada grupo
table(hdb$cluster)

# Visualiza grupos
fviz_cluster(list(data=dados, cluster=hdb$cluster), main="")

# Aplica a técnica de agrupamento espectral
spec <- specc(as.matrix(dados), centers=4)

# Rótulos dos grupos
clusters_spec <- as.numeric(spec)

# Adiciona os rótulos dos agrupamentos ao conjunto de dados original
df_spec <- base %>% mutate(cluster=clusters_spec)

# Visualiza grupos
fviz_cluster(list(data=dados, cluster=clusters_spec), main="")

# Aplica a técnica de propagação por afinidade
ap <- apcluster(negDistMat(as.matrix(dados), r=2))

# Grupos encontrados
ap

# Rótulos dos grupos
clusters_ap <- labels(ap)

# Adiciona os rótulos dos agrupamentos ao conjunto de dados original
df_ap <- base %>% mutate(cluster=clusters_ap)

# Número de grupos
length(unique(clusters_ap))

# Índices dos exemplares
ap@exemplars

# Paı́ses exemplares
paises[ap@exemplars]

# Visualiza grupos
fviz_cluster(list(data=dados, cluster=clusters_ap), main="")



# Carrega as bibliotecas
library(tidyverse)
library(factoextra)
library(cluster)
library(dbscan)
library(apcluster)

# Lê o conjunto de dados
dados <- read.csv("covtype.csv", header=TRUE)

# Seleciona aleatoriamente 10.000 amostras do conjunto de dados
set.seed(123)
amostras_dados <- dados %>% sample_n(10000)

# Remove a variável alvo (rótulo), mantendo apenas os atributos
x <- amostras_dados %>% select(-Cover_Type)

# Remove colunas com variância zero (constantes) para evitar problemas na normalização
x <- x[, apply(x, 2, var) != 0]

# Normaliza os dados
x_scaled <- scale(x)

# Aplica a técnica K-Means
set.seed(123)
k <- 7
kmeans_model <- kmeans(x_scaled, centers=k, nstart=25)

# Mostra a quantidade de amostras em cada grupo
table(kmeans_model$cluster)

# Visualiza os agrupamentos
fviz_cluster(kmeans_model, data=x_scaled, main = "")

# Aplica a técnica HDBSCAN
hdb_model <- hdbscan(x_scaled, minPts=15)

# Mostra a quantidade de amostras em cada grupo (incluindo outliers)
table(hdb_model$cluster)

# Conta quantas amostras foram classificadas como outliers (cluster 0)
sum(hdb_model$cluster == 0)

# Visualiza os agrupamentos
fviz_cluster(list(data=x_scaled, cluster=hdb_model$cluster), 
             main="")


# Aplica a técnica de propagação por afinidade
sim <- negDistMat(x_scaled, r=2)
ap_model <- apcluster(sim, p=quantile(sim, 0.01))

# Retorna o número de grupos encontrados
length(ap_model)

# Retorna o tamanho (número de amostras) de cada grupo identificado
sapply(slot(ap_model, "clusters"), length)

# Visualiza os agrupamentos
clusters_ap <- labels(ap_model)
fviz_cluster(list(data=x_scaled, cluster=clusters_ap), main="") + 
  guides(shape="none")

