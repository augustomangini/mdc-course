# ---
# title: INF0613 -- Aprendizado de Máquina Não Supervisionado
# output: pdf_document
# subtitle: Trabalho 1 - Regras de Associação
# author: 
#   - Nome completo Integrante 1
#   - Nome completo Integrante 2
#   - Nome completo Integrante 3
# ---


# ```{r setup, include = FALSE}
# nitr::opts_chunk$set(echo = TRUE, error = FALSE, message = FALSE, warning = FALSE, tidy = FALSE)
options(digits = 3)
# ```

# Neste primeiro trabalho, vamos minerar Regras de Associação em uma base de dados que contém as vendas de uma padaria. A base de dados está 
# disponível na página da disciplina no Moodle (arquivo `bakery.csv`).

# Atividade 0 -- Configurando o ambiente
# Antes de começar a implementação do seu trabalho configure o _workspace_ e importe todos os pacotes:

# ```{r atv0-code}
# Adicione os demais pacotes usados
# Bibliotecas usadas neste trabalho:
library(arules)

# Configurando ambiente de trabalho:
# setwd("")
setwd("/home/julio/Área de trabalho/APRENDIZADO DE MÁQUINA NÃO SUPERVISIONADO/Trabalho 1") 


# Atividade 1 -- Análise Exploratória da Base de Dados (*3,0 pts*)

# Dado um caminho para uma base de dados, leia as transações e faça uma análise Exploratória sobre elas. Use as funções 
# `summary`,  `inspect` e `itemFrequencyPlot`. Na função `inspect`, limite sua análise às 10 primeiras transações e na 
# função `itemFrequencyPlot` gere um gráfico com a frequência relativa dos 30 itens mais frequentes. 

# ```{r atv1-code}
# Ler transações
transacoes <- read.transactions("bakery.csv", format="basket", sep=",")

# Visualizando transações
inspect(transacoes[1:10])

# Sumário da base
summary(transacoes)

# Analisando a frequência dos itens 
itemFrequencyPlot(transacoes, topN = 30, type = "relative")


## Análise 

# a) Descreva a base de dados discutindo os resultados das funções acima. 

# **Resposta:** <!-- Escreva sua resposta abaixo -->

# A base contém 2579 (linhas) transações e 91 (colunas) itens distintos, apresentando baixa densidade (0.0352), indicando que 
# cada transação possui poucos itens. Os itens mais frequentes são Coffee, Bread e Tea, com destaque para 
# Coffee, que aparece em mais da metade das transações.
# A maioria das transações contém entre 2 e 4 itens, com média de 3.2 itens por compra.
# A análise das primeiras transações mostra padrões típicos de consumo, como combinações entre bebidas 
# (Coffee, Tea) e produtos de padaria (Bread, Muffin, Cake), sugerindo possíveis associações entre esses itens.

#<!-- Fim da resposta -->

# b) Ao gerarmos o gráfico de frequências, temos uma representação visual de uma informação já presente no resultado
# da função `summary`. Contudo, esse gráfico nos dá uma visão mais ampla da base. Assim, podemos ver a frequência de 
# outros itens em relação aos 10 mais frequentes. Quais informações podemos obter a partir desse gráfico (e da análise 
# anterior) para nos ajudar na extração de regras de associação com o algoritmo `apriori`? Isto é, como a frequência 
# dos itens pode afetar os parâmetros de configuração do algoritmo `apriori`? 

# **Resposta:** <!-- Escreva sua resposta abaixo -->

# O gráfico de frequência evidencia que poucos itens concentram grande parte das ocorrências, com destaque para 
# Coffee, Bread e Tea, enquanto a maioria dos itens apresenta frequência inferior. Essa distribuição desigual 
# caracteriza uma base com forte concentração em poucos itens e uma grande quantidade de itens pouco frequentes.
# Com isso, podemos concluir que definir um suporte mínimo muito alto no algoritmo apriori pode resultar na 
# geração de poucas regras, limitando a descoberta de associações entre itens menos frequentes. Por exemplo,
# um suporte acima de 0.2 consideraria basicamente apenas os itens mais populares, excluindo grande parte da
# base e reduzindo a diversidade de padrões encontrados.

# <!-- Fim da resposta -->

# Atividade 2 -- Minerando Regras (*3,5 pts*)

# Use o algoritmo `apriori` para minerar regras na base de dados fornecida. Experimente com pelo menos *3 conjuntos* 
# de valores diferentes de suporte e confiança para encontrar regras de associação. Imprima as cinco regras com o 
# maior confiança de cada conjunto escolhido.  Lembre-se de usar seu conhecimento sobre a base, obtido na questão 
# anterior, para a escolha dos valores de suporte e confiança.

# ```{r atv2-code}
# Conjunto 1: suporte = 0.02   e confiança = 0.6
# Pega apenas itens mais frequentes e regras com alta confiança, resultando em um conjunto de regras mais restrito 
# e confiável
regra_conjunto_1 <- apriori(transacoes, parameter = list(supp = 0.01, conf = 0.6))

regra_conjunto_1

inspect(regra_conjunto_1[1:5])

# Conjunto 2: suporte = 0.01    e confiança = 0.5
# Começa a capturar itens menos frequentes mas que ainda apresentam uma boa associação
regra_conjunto_2 <- apriori(transacoes, parameter = list(supp = 0.01, conf = 0.5))

regra_conjunto_2

inspect(regra_conjunto_2[1:5])

# Conjunto 3: suporte = 0.03   e confiança = 0.7
# Descobre padrões menos óbvios, mas que podem ser interessantes
regra_conjunto_3 <- apriori(transacoes, parameter = list(supp = 0.001, conf = 0.7))

regra_conjunto_3

inspect(regra_conjunto_3[1:5])

# ```

## Análises 
# a) Quais as regras mais interessantes geradas a partir dessa base? Justifique.

# **Resposta:** <!-- Escreva sua resposta abaixo -->

# A regra mais relevante identificada foi {Toast} => {Coffee}, com suporte de 0.0399 
# (103 ocorrências) e confiança de 72%. Essa regra indica que, em 72% das transações 
# que incluem Toast, também ocorre a compra de Coffee. O conjunto de parâmetros utilizado 
# (suporte = 0.01 e confiança = 0.6) gerou um total de 10 regras, permitindo identificar 
# padrões consistentes sem gerar excesso de associações irrelevantes.
# Essa regra se destaca por apresentar um bom equilíbrio entre suporte e confiança, 
# sendo baseada em um número significativo de transações, o que aumenta sua confiabilidade. 
# Além disso, revela um padrão claro de consumo que pode ser explorado em estratégias de 
# marketing, como promoções combinadas.
# Outra regra interessante é {Salad} => {Coffee}, com suporte de 0.0174 (45 ocorrências) 
# e confiança de 68.2%, obtida com suporte = 0.01 e confiança = 0.5. De forma semelhante, 
# a regra {Spanish Brunch} => {Coffee}, com suporte de 0.0252 (65 ocorrências) e confiança 
# de 60.7%, também evidencia um padrão relevante de consumo.
# Essas regras apresentam níveis consistentes de suporte e confiança, indicando associações 
# frequentes entre os itens analisados.

# <!-- Fim da resposta -->

# Atividade 3 -- Medidas de Interesse (*3,5 pts*)

# Vimos na aula que, mesmo após as podas do algoritmo `apriori`, ainda temos algumas regras com características 
# indesejáveis como redundâncias e dependência estatística negativa. Também vimos algumas medidas que nos ajudam 
# a analisar melhor essas regras como o lift, a convicção e a razão de chances. Nesta questão, escolha um dos 
# conjuntos de regras geradas na atividade anterior e o analise usando essas medidas. Compute as três medidas 
# para o conjunto escolhido com a função `interestMeasure` e experimente ordenar as regras com cada uma das novas medidas.

# Dica: para adicionar as medidas em um conjunto de regras qualquer, você pode utilizar o comando `cbind` e a função `quality`:
# ```
# quality(regras) <- cbind(quality(regras), interestMeasure(regras, measure=c("conviction", "oddsRatio"), 
#                                          transactions = transacoes))
#```


# ```{r atv3-code}
# Compute as medidas de interesse 

# O lift já esta presente no resultado do apriori, então vamos calcular apenas a convicção e a razão de chances
quality(regra_conjunto_1) <- cbind(quality(regra_conjunto_1), interestMeasure(regra_conjunto_1, measure=c("conviction", "oddsRatio"), 
                                          transactions = transacoes))

head(quality(regra_conjunto_1))                                          

# Apresente as regras ordenadas por lift

inspect(sort(regra_conjunto_1, by = "lift")[1:5])
#     lhs                        rhs      support confidence coverage lift count conviction oddsRatio
# [1] {Toast}                 => {Coffee} 0.0399  0.720      0.0554   1.32 103   1.63       2.25     
# [2] {Cake, Sandwich}        => {Coffee} 0.0143  0.685      0.0209   1.26  37   1.45       1.85     
# [3] {Salad}                 => {Coffee} 0.0174  0.682      0.0256   1.25  45   1.43       1.82     
# [4] {Hot chocolate, Pastry} => {Coffee} 0.0109  0.667      0.0163   1.23  28   1.37       1.69     
# [5] {Sandwich, Soup}        => {Coffee} 0.0105  0.628      0.0167   1.15  27   1.23       1.42  

# Apresente as regras ordenadas por convicção

inspect(sort(regra_conjunto_1, by = "conviction")[1:5])

#     lhs                        rhs      support confidence coverage lift count conviction oddsRatio
# [1] {Toast}                 => {Coffee} 0.0399  0.720      0.0554   1.32 103   1.63       2.25     
# [2] {Cake, Sandwich}        => {Coffee} 0.0143  0.685      0.0209   1.26  37   1.45       1.85     
# [3] {Salad}                 => {Coffee} 0.0174  0.682      0.0256   1.25  45   1.43       1.82     
# [4] {Hot chocolate, Pastry} => {Coffee} 0.0109  0.667      0.0163   1.23  28   1.37       1.69     
# [5] {Sandwich, Soup}        => {Coffee} 0.0105  0.628      0.0167   1.15  27   1.23       1.42 

# Apresente as regras ordenadas por razão de chances

inspect(sort(regra_conjunto_1, by = "oddsRatio")[1:5])

#     lhs                        rhs      support confidence coverage lift count conviction oddsRatio
# [1] {Toast}                 => {Coffee} 0.0399  0.720      0.0554   1.32 103   1.63       2.25     
# [2] {Cake, Sandwich}        => {Coffee} 0.0143  0.685      0.0209   1.26  37   1.45       1.85     
# [3] {Salad}                 => {Coffee} 0.0174  0.682      0.0256   1.25  45   1.43       1.82     
# [4] {Hot chocolate, Pastry} => {Coffee} 0.0109  0.667      0.0163   1.23  28   1.37       1.69     
# [5] {Sandwich, Soup}        => {Coffee} 0.0105  0.628      0.0167   1.15  27   1.23       1.42  

## Análise 
# a) Quais as regras mais interessantes do conjunto? Justifique.

# **Resposta:** <!-- Escreva sua resposta abaixo -->

# Foi utilizado o conjunto de regras gerado com suporte = 0.01 e confiança = 0.6, que apresentou um total de 10 regras. 
# A regra mais interessante identificada foi {Toast} => {Coffee}, com suporte de 0.0399 (103 ocorrências) e confiança de 72%.
# 
# Após o cálculo das medidas de interesse, essa regra se destaca por apresentar lift de 1.32, indicando que a presença 
# de Toast aumenta a probabilidade de ocorrência de Coffee em relação à sua frequência geral na base. Além disso, a convicção 
# de 1.63 sugere que a regra possui boa confiabilidade, sendo menos suscetível a falhas, enquanto a razão de chances de 2.25 
# reforça a existência de uma associação positiva entre os itens.
# 
# As três ordenações realizadas (por lift, convicção e razão de chances) destacaram as mesmas regras, evidenciando consistência 
# entre as métricas e reforçando a relevância das associações encontradas. Além da regra principal, também se destacam 
# {Cake, Sandwich} => {Coffee} e {Salad} => {Coffee}, que apresentam valores elevados nessas medidas, indicando padrões de 
# consumo significativos entre os itens analisados.

# <!-- Fim da resposta -->

