########################################
# Trabalho 1 - INF-0612          
# Nome(s): 
########################################

########################################
# 1) Distribuidora
########################################

## Os vetores C, L e V representam os produtos distribuidos nas cidades de Campinas, Limeira e Vinhedo, respectivamente.

C <- c("Xampu", "Sabonete", "Arroz", "Chocolate", "Leite", "Refrigerante", "Queijo", "Suco", "Vinho", "Cerveja")
L <- c("Leite", "Cerveja", "Arroz", "Chocolate")
V <- c("Sabonete", "Detergente", "Refrigerante", "Carne", "Vinho", "Chocolate", "Papel", "Leite", "Iogurte")

## Perguntas:
## Quais os produtos que sao vendidos em Campinas, mas nao sao vendidos em Limeira?
Q1 <- setdiff(C, L)

## Quais os produtos que sao vendidos em Vinhedo, mas nao sao vendidos em Campinas?
Q2 <- setdiff(V, C)

## Quais os produtos que sao vendidos em pelo menos uma cidade?
Q3 <- union(C, union(L, V))

## Quais os produtos que sao vendidos em todas as cidades?
Q4 <- intersect(C, intersect(L, V))

## Se a filial de Limeira parar a distribuicao de produtos, a filial de Campinas 
## terá todos os itens necessarios para atender à demanda de Limeira? 
Q5 <- setdiff(L, C)

########################################
# 2) Criptografia XOR
########################################

bin <- c(1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0)
key <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

## Você deve encriptar o número binário representado pelo vetor bin, utilizando como chave o número binário representado pelo vetor key O resultado deve ser salvo no vetor enc, utilizando a representação em vetor com elementos 0 e 1. Note que embora exista uma função XOR em R, você não deve utilizá-la para resolver este problema. Você pode usar as operações lógicas vistas em aula, como os operadores !, & e `|.
enc <- as.numeric((bin | key) & !(bin & key))

## Você deve converter o vetor bin para um número decimal e salvar o resultado na variável dec.
dec <- sum(2^sort(1:length(bin)-1, decreasing = TRUE)*bin)

## Você deve converter o vetor enc para um número decimal e salvar o resultado na variável enc_dec.
enc_dec <- sum(2^sort(1:length(enc)-1, decreasing = TRUE)*enc)

########################################
# 3) Média Final
########################################

ids <- c(172742, 172773, 172825, 172839, 172967, 173149, 173204, 173370, 174096, 174355, 174437, 174487, 174488, 174928, 175380, 175832, 176859, 176914, 176940, 177388, 177554, 177609, 177643, 177825, 177925, 178085, 178137, 178377, 178397, 178525, 178664, 178674, 178740, 178779, 181987, 182039, 182049, 182901, 183024, 183143, 183517, 183984, 184400, 185247, 185820, 186218, 187014, 187217, 188078, 188494, 188548)

p1 <- c(6, 5.1, 7, 3.9, 8.9, 2.6, 0, 0.5, 3.3, 8, 4.8, 4.4, 3.1, 8.3, 1.4, 0.5, 1.1, 5.8, 9.5, 5.1, 4.3, 7.5, 4.8, 1.8, 5.4, 9.5, 4.8, 4.8, 3.3, 8.4, 4.4, 7.8, 8.8, 8.9, 0, 7, 6.9, 5.8, 6.3, 8.8, 6.3, 8.6, 6.1, 5, 3.8, 4.5, 5.4, 8, 1.9, 1.6, 1.6)

p2 <- c(8.6, 8.5, 6.8, 9.1, 5.3, 4.8, 0, 0, 0, 8.9, 6.2, 6.5, 2.9, 6.8, 3.5, 4.8, 6.1, 7.8, 8.9, 8, 5.6, 7.2, 8.9, 0, 7.6, 9.8, 3.3, 8.2, 7.6, 8.8, 0, 9.4, 9.3, 8.1, 0, 8, 9.6, 0, 8.9, 9, 4.5, 5.5, 6.8, 8.6, 0, 9.6, 6.8, 7.8, 5.9, 2.9, 2.3)

p3 <- c(7.6, 7.1, 6.9, 7, 6.7, 3.9, 0, 0.2, 1.3, 8.5, 5.6, 5.6, 3, 7.4, 2.7, 3.1, 4.1, 7, 9.1, 6.9, 5.1, 7.3, 7.2, 0.7, 6.7, 9.6, 3.9, 6.8, 5.9, 8.6, 1.8, 8.7, 9.1, 8.4, 0, 7.6, 8.5, 2.3, 7.8, 8.9, 5.2, 6.8, 6.5, 7.2, 1.5, 7.6, 6.2, 7.9, 4.3, 2.4, 2)

p4 <- c(9.9, 2.3, 10, 7.3, 8.6, 8.9, 0, 9.5, 4.5, 7.9, 8.9, 8.6, 8.2, 6.4, 2.7, 10, 8.6, 8.3, 9.4, 9.5, 8.6, 9.5, 9.1, 0, 10, 7.8, 9.9, 8.2, 6.8, 8.7, 3.2, 6.9, 6.3, 8.9, 3.2, 10, 5.3, 6.4, 7.9, 7.8, 8.2, 8.6, 7.3, 7.5, 5, 8.3, 10, 4.8, 10, 6.4, 7.9)

## Você pode utilizar a função abaixo. Ela retorna, dada uma matriz
## (ou estrutura tabular), o menor elemento linha a linha. Por exemplo,
## dada uma matriz m, para obter o menor elemento linha a linha entre
## as colunas 2 e 3, devemos utilizar o comando rowMins(m[,c(2:3)]).

rowMins <- function(m) {
 apply(m, 1, min)
}

## Perguntas:
## Você deve criar, na variável alunos, um data frame utilizando os vetores fornecidos. Além disso, sempre que utilizar algum dado já existente no arquivo, você deve referir-se a esse data frame (ou seja, você só pode utilizar os vetores fornecidos para criar esse data frame).
alunos <-data.frame(AlunoID = ids, p1, p2, p3, p4)

## Você deve salvar no vetor medquad a média final de cada aluno (média quadrática das provas, desconsiderando a menor nota obtida), com duas casas decimais.
minValues <- rowMins(alunos)
quadraticSums <- rowSums(alunos[, 2:5]^2)-minValues^2
medquad <- round(sqrt(quadraticSums/(length(alunos)-1)), digits = 2); medquad

## Você deve salvar no vetor reprovados um valor booleano correspondente a cada aluno, de forma que alunos com média final inferior a 7 sejam considerados reprovados (valor TRUE) e os demais sejam considerados aprovados (valor FALSE).
reprovados <- medquad < 7; reprovados #Deve conter os IDs dos alunos?

## Considerando apenas os alunos aprovados. Você deve salvar nas variáveis mp1, mp2, mp3 e mp4 a média aritmética das notas das provas 1, 2, 3 e 4, respectivamente.
mp1 <- mean(alunos[!reprovados,2]) #Confirmar se está correto
mp2 <- mean(alunos[!reprovados,3])
mp3 <- mean(alunos[!reprovados,4])
mp4 <- mean(alunos[!reprovados,5])

## Considerando apenas os alunos aprovados. Você deve salvar nas variáveis dp1, dp2, dp3 e dp4 o desvio padrão das notas das provas 1, 2, 3 e 4, respectivamente.
dp1 <- sd(alunos[!reprovados,2]) #Confirmar se está correto
dp2 <- sd(alunos[!reprovados,3])
dp3 <- sd(alunos[!reprovados,4])
dp4 <- sd(alunos[!reprovados,5])

########################################
# 4) Chuvas
########################################

dia <- c(01, 01, 01, 02, 02, 02, 02, 03, 03, 03, 04, 04, 04, 05, 05, 06, 06, 06, 06, 07, 07, 07, 07, 07, 08, 08, 08, 08, 09, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15)

cidade <- c('Campinas', 'Limeira', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Vinhedo', 'Campinas', 'Vinhedo', 'Vinhedo', 'Limeira', 'Limeira', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Vinhedo', 'Campinas', 'Vinhedo', 'Vinhedo', 'Campinas', 'Limeira', 'Vinhedo', 'Limeira', 'Vinhedo', 'Campinas', 'Limeira', 'Vinhedo', 'Limeira', 'Campinas', 'Limeira', 'Limeira', 'Campinas', 'Campinas', 'Limeira', 'Limeira')

chuva <- c(0.15, 0.11, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45, 2.63, 0.76, 0.38, 1.26, 2.57, 0.54, 9.87, 3.41, 2.96, 1.37, 6.78, 13.87, 0.11, 1.84, 12.19, 2.86, 11.99, 2.01, 9.32, 11.2, 0.48, 10.33, 13.32, 13.87, 1.05, 0.56, 1.92, 1.81, 7.66, 2.93, 1.17, 0.7, 2.95, 0.37, 1.08, 1.31, 3.22, 0.49, 1.86, 2.3, 7.65)


## Perguntas:
## Você deve criar, na variável leituras, um data frame utilizando os vetores fornecidos e, sempre que utilizar algum dado desses vetores, referir-se apenas a esse data frame (ou seja, você só pode utilizar os vetores fornecidos para criar esse data frame).
leituras <- data.frame(dia, cidade, chuva); leituras

## Remova do data frame leituras todas as linhas que possuem o mesmo dia e cidade, mantendo apenas a última ocorrência. Por exemplo, considere o data frame abaixo:
leituras <- leituras[!duplicated(leituras[c("dia", "cidade")], fromLast = TRUE),]; leituras


## Salve nas variáveis acumCamp, acumLim e acumVin o total de chuvas observados nos 15 dias nas cidades de Campinas, Limeira e Vinhedo, respectivamente.
acumCamp <- sum(leituras[leituras$cidade=="Campinas", "chuva"])
acumLim <- sum(leituras[leituras$cidade=="Limeira", "chuva"])
acumVin <- sum(leituras[leituras$cidade=="Vinhedo", "chuva"])

## Você deve salvar nas variáveis dmaxCamp, dmaxLim e dmaxVin, dentre os dados existentes em seu data frame, o dia do mês com maior leitura de chuva nas cidades de Campinas, Limeira e Vinhedo, respectivamente. Se existir mais de um dia com o valor máximo, você deve escolher o primeiro dia. Caso uma cidade não tenha leitura em algum dia, aquele dia deve ser ignorado.
dmaxCamp <- acumCamp$dia[order(acumCamp$chuva, decreasing=TRUE)[1]]
dmaxLim <- acumLim$dia[order(acumLim$chuva, decreasing=TRUE)[1]]
dmaxVin <- acumVin$dia[order(acumVin$chuva, decreasing=TRUE)[1]]


## Você deve salvar nas variáveis dminCamp, dminLim e dminVin, dentre os dados existentes em seu data frame, o dia do mês com menor leitura de chuva nas cidades de Campinas, Limeira e Vinhedo, respectivamente. Se existir mais de um dia com o valor mínimo, você deve escolher o primeiro dia. Caso uma cidade não tenha leitura em algum dia, aquele dia deve ser ignorado.
dminCamp <- acumCamp$dia[order(acumCamp$chuva)[1]]
dminLim <- acumLim$dia[order(acumLim$chuva)[1]]
dminVin <- acumVin$dia[order(acumVin$chuva)[1]]
