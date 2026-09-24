### Base de Dados Iris
?iris

class(iris)

summary(iris)

# Número total de flores
nrow(iris)

### Comprimento da Sépala

q <- quantile(iris$Sepal.Length); q

# Flores com as sépalas entre as 25% mais curtas 
bottom25pc <- iris[["Sepal.Length"]] < q["25%"]
iris[bottom25pc, ]

# Flores com as sépalas entre as 25% mais compridas 
top25pc <- iris[["Sepal.Length"]] > q["75%"]
iris[top25pc, ]

### Largura da Sépala

q <- quantile(iris[["Sepal.Width"]], (1:10)*0.1); q

# Flores com as sépalas entre as 10% mais estreitas 
bottom10pc <- iris[["Sepal.Width"]] < q["10%"]
iris[bottom10pc, ]

# Flores com as sépalas entre as 10% mais largas
top10pc <- iris[["Sepal.Width"]] > q["90%"]
iris[top10pc, ]

### Comprimento da Pétala

# Flores com as pétalas entre as 10 mais curtas 
bottom10 <- order(iris$Petal.Length)[1:10]
iris[bottom10, ]

# Flores com as pétalas entre as 10 mais compridas 
top10 <- rev(order(iris$Petal.Length))[1:10]
iris[top10, ]

# Flores ordenadas pelo comprimento da pétala
iris[order(iris$Petal.Length), ]

# Posição relativa média das flores de cada espécie em relação ao comprimento da pétala
mean(rank(iris$Petal.Length)[iris$Species == "setosa"])
mean(rank(iris$Petal.Length)[iris$Species == "versicolor"])
mean(rank(iris$Petal.Length)[iris$Species == "virginica"])

### Largura da Pétala

# Flores que possuem larguras de pétalas distintas em relação a todas as demais
Width.values <- unique(iris$Petal.Width)
Width.duplicated <- unique(iris$Petal.Width[duplicated(iris$Petal.Width)])
Width.single <- setdiff(Width.values, Width.duplicated)

iris[is.element(iris$Petal.Width, Width.single), ]

### Geral

# Correlação entre as medidas das flores
cor(iris[ , 1:4])
cor.Petal <- cor(iris$Petal.Length, iris$Petal.Width)
cor.Sepal <- cor(iris$Sepal.Length, iris$Sepal.Width)

# As medidas das pétalas são mais correlacionadas que as medidas das sépalas?
cor.Petal > cor.Sepal

# Flores com pétalas entre as 10 mais largas e ao mesmo tempo entre as 10 mais compridas
top10.Petal.Length <- rev(order(iris$Petal.Length))[1:10]
top10.Petal.Width <- order(iris$Petal.Width, decreasing = TRUE)[1:10]
top10.Petal <- intersect(top10.Petal.Length, top10.Petal.Width)

iris[top10.Petal, ]

# Data frames apenas com as flores daquela espécie
setosa <- iris[iris$Species == "setosa", ]
versicolor <- iris[iris$Species == "versicolor", ]
virginica <- iris[iris$Species == "virginica", ]

# Flores com largura da sépala com diferença de
# mais de dois desvios padrões em relação a média
mean.Sepal.Width <- mean(iris$Sepal.Width)
sd.Sepal.Width <- sd(iris$Sepal.Width)

# Quantas são as flores com a propriedade acima?
sum(abs(iris$Sepal.Width - mean.Sepal.Width) > 2 * sd.Sepal.Width)

# Quais as flores com a propriedade acima?
iris[abs(iris$Sepal.Width - mean.Sepal.Width) > 2 * sd.Sepal.Width, ]

# Ordenação pelo comprimento da pétala
iris[order(iris$Petal.Length), ]

# Quantas das 50 flores com menores comprimentos de pétalas são setosas?
bottom50.Petal.Length <- order(iris$Petal.Length)[1:50]
sum(iris[bottom50.Petal.Length, ]$Species == "setosa")

# Quantas das 50 flores com maiores comprimentos de pétalas são virginicas?
top50.Petal.Length <- rev(order(iris$Petal.Length))[1:50]
sum(iris[top50.Petal.Length, ]$Species == "virginica")

# Flores com largura das pétalas abaixo da média
bottom.Petal.Width <- (iris$Petal.Width < mean(iris$Petal.Width))

# Flores com comprimento das pétalas acima da média
top.Petal.Lenght <- (iris$Petal.Length > mean(iris$Petal.Length))

# Existe alguma flor com comprimento das pétalas acima da média 
# e com largura das pétalas abaixo da média?
any(bottom.Petal.Width & top.Petal.Lenght)

# Quais são as flores com a propriedade anterior?
iris[bottom.Petal.Width & top.Petal.Lenght, ]

# Todas as flores tem sépalas mais compridas que as suas pétalas? 
all(iris$Sepal.Length > iris$Petal.Length)

# Todas as flores tem pétalas mais compridas do que largas? 
all(iris$Petal.Length > iris$Petal.Width)

# Existe alguma flor com sépala pelo menos 4x mais comprida que sua pétala?
any(iris$Sepal.Length >= 4 * iris$Petal.Length)

# Quais são as flores com a propriedade anterior?
iris[iris$Sepal.Length >= 4 * iris$Petal.Length, ]

# Definindo a razão das medidas das sépalas e das pétalas
iris$Sepal.Ratio <- iris$Sepal.Length / iris$Sepal.Width
iris$Petal.Ratio <- iris$Petal.Length / iris$Petal.Width

# Quais é a flor com maior razão das medidas das sépalas?
iris[iris$Sepal.Ratio == max(iris$Sepal.Ratio), ]

# É verdade que existe uma flor que possui a menor razão entre as
# dimensões da sépala e maior razão entre as dimensões da pétala?
any(iris$Sepal.Ratio == min(iris$Sepal.Ratio) & 
      iris$Petal.Ratio == max(iris$Petal.Ratio))

# Que flor possui a propriedade acima?
iris[iris$Sepal.Ratio == min(iris$Sepal.Ratio) &
       iris$Petal.Ratio == max(iris$Petal.Ratio), ]

## Dados agrupados por espécie

# Média dos comprimentos das pétalas de cada espécie
mean(iris$Petal.Length)
mean(setosa$Petal.Length)
mean(versicolor$Petal.Length)
mean(virginica$Petal.Length)

# Usando função de agrupamento para a mesma análise sem a 
# necessidade de dividir os dados em múltiplos dataframes
tapply(iris$Petal.Width, iris$Species, mean)
tapply(iris$Petal.Ratio, iris$Species, mean)

# ... ou sem a necessidade de se criar novas colunas
tapply(iris$Petal.Length / iris$Petal.Width, 
       iris$Species, mean)
