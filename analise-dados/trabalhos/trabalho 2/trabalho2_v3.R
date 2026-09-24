########################################
# Trabalho 2 - INF-0612
# Nome(s):
# Anselmo Faria Alvarez Júnior
# Augusto José Mangini dos Santos
# Julio Cesar da Silva
########################################

library(ggplot2)

# Extraindo os dados do cepagri
names <- names <- c("horario", "temperatura", "vento", "umidade", "sensação")
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.csv(con, header = FALSE, sep = ";", col.names = names)


# Tratamento de dados

# 1) Transformando para datas
cepagri$horario <- as.POSIXct(cepagri$horario,
                              format = "%d/%m/%Y-%H:%M",
                              tz = "America/Sao_Paulo")


# 2) Selecionando apenas horario >= 01/01/2015 00:00:00 e < 01/01/2026 00:00:00
cepagri <- cepagri[cepagri$horario >= "2015-01-01 00:00:00" &
               cepagri$horario < "2026-01-01 00:00:00", ]


# 3) Excluindo as linhas onde a temperatura tem um [ERRO] e transformando a
# coluna em numeric
cepagri <- cepagri[cepagri$temperatura != " [ERRO]",]


# 4) Transformação dos dados em numeric
cepagri$temperatura <- as.numeric(cepagri$temperatura)
cepagri$vento       <- as.numeric(cepagri$vento)
cepagri$umidade     <- as.numeric(cepagri$umidade)
cepagri$sensação    <- as.numeric(cepagri$sensação)


# 5) Outliers 
# -> temp negativa -> vento forte e tempestade -> não parece outlier
# sensação de 99 é erro -> todas no mesmo horário]
# como já tem vários NAs nessa coluna, substituir o 99 por NA
cepagri[!is.na(cepagri$sensação) & (cepagri$sensação == 99.9), 5] <- NA


# 6) entradas com temp. e umidade de -7999, podemos excluir
cepagri <- cepagri[cepagri$temperatura != -7999,]


# 7) Umidade do ar a 0% -> acima de 0 o menor valor é 8,8% e é fisicamente
# impossível a umidade do ar chegar a 0% então substituir por NA
cepagri[!is.na(cepagri$umidade) & (cepagri$umidade == 0), 4] <- NA


# 8) Sequência de dados duplicados -> mais de 24 horas de dados duplicados
consecutive <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  return(result)
}
filtro <- consecutive(cepagri$temperatura, 144)
datas_duplicadas <- unique(as.Date(cepagri[filtro, 1]))
cepagri <- cepagri[!as.Date(cepagri$horario) %in% datas_duplicadas, ]

# 9) Adicionando mês e ano para auxiliar nas análises
cepagri$ano <- as.numeric(format(cepagri$horario, "%Y"))
cepagri$mes <- as.numeric(format(cepagri$horario, "%m"))

#==========================================================================================
# Analise de dados
#==========================================================================================

# 1) Analise de umidade

# Criando um df auxiliar por dia (sem horario) com a umidade minima e conta os dias
# com umidade baixa
cepagri_dia <- cepagri
cepagri_dia$data <- as.Date(cepagri$horario)

# agregando os dados pelo minimo de cada dia e excluindo coluna de agrupamento
umidade_por_dia <- aggregate(x = cepagri_dia, list(cepagri_dia$ano, cepagri_dia$data),
                         FUN = min, na.rm = TRUE)
umidade_por_dia <- umidade_por_dia[, -c(1, 2, 3)]

#filtrando os dias para umidade em atenção e mantendo apenas as colunas de interesse
dias_filtrados_baixa <- umidade_por_dia[umidade_por_dia$umidade >= 12 & umidade_por_dia$umidade < 30, ]
tabela_umidade_baixa <- aggregate(x = dias_filtrados_baixa, list(dias_filtrados_baixa$ano), 
                                  FUN = length)
tabela_umidade_baixa <- tabela_umidade_baixa[, -c(3, 4, 5, 6, 7, 8)]
colnames(tabela_umidade_baixa) <- c("ano", "dias_umidade_baixa")

# fazendo o mesmo processo para umidade abaixo de 12%
dias_filtrados_critica <- umidade_por_dia[umidade_por_dia$umidade < 12, ]
tabela_umidade_critica <- aggregate(x = dias_filtrados_critica, list(dias_filtrados_critica$ano), 
                                    FUN = length)
tabela_umidade_critica <- tabela_umidade_critica[, -c(3, 4, 5, 6, 7, 8)]
colnames(tabela_umidade_critica) <- c("ano", "dias_umidade_baixa")

# Unificando as tabelas
tabela_umidade <- merge(tabela_umidade_baixa, tabela_umidade_critica, by = "ano", all=TRUE)
colnames(tabela_umidade) <- c("ano", "dias_umidade_atenção", "dias_umidade_critica")
tabela_umidade[is.na(tabela_umidade)] <- 0

# Analisando a distribuição desses dias ao longo dos meses
# criando para umidade baixa
dias_baixa_mes <- aggregate(x = dias_filtrados_baixa,
                            list(dias_filtrados_baixa$mes),
                            FUN = length)
dias_baixa_mes <- dias_baixa_mes[, -c(3, 4, 5, 6, 7, 8)]
colnames(dias_baixa_mes) <- c("mes", "dias_umidade_baixa")
dias_baixa_mes$faixa <- "Igual ou acima 12% e abaixo de 30%"

# criando para umidade critica
dias_critica_mes <- aggregate(x = dias_filtrados_critica,
                            list(dias_filtrados_critica$mes),
                            FUN = length)
dias_critica_mes <- dias_critica_mes[, -c(3, 4, 5, 6, 7, 8)]
colnames(dias_critica_mes) <- c("mes", "dias_umidade_baixa")
dias_critica_mes$faixa <- "Abaixo de 12%"

# Juntando os dois dataframes (formato appendado):
tabela_mes <- rbind(dias_baixa_mes, dias_critica_mes)
colnames(tabela_mes) <- c("mes", "dias", "faixa")

# Convertendo para o nome do mês
tabela_mes$mes <- factor(month.abb[tabela_mes$mes],
                         levels = month.abb,
                         ordered = TRUE)

# Grafico - geom_bar
# o fill funciona devido ao rbind anterior empilhar as faixas
p <- ggplot(tabela_mes, aes(x = mes, y = dias, fill = faixa))

#O stat = "identity" no geom_bar é necessario porque os valores ja sao calculados
p <- p + geom_bar(stat = "identity")

# Preenchendo com cores
p <- p + scale_fill_manual(values = c("Entre 12% e 30%" = "#F4A460",
                                      "Abaixo de 12%"   = "#B22222"))
# Adicionando legendas aos eixos
p <- p + labs(x = "Mês", y = "Número de dias",
              fill = "Faixa de umidade",
              title = "Dias de baixa umidade por mês (2015–2025)")
# Tema
p <- p + theme_minimal()
p <- p + theme(plot.title = element_text(hjust = 0.5))
# Adicionando rotulo de dados ao grafico - escondendo o rotulo dos dias menores
# que 3 para não ficar esprimido no gráfico
p <- p + geom_text(aes(label = ifelse(dias >= 3, dias, "")),
                   position = position_stack(vjust = 0.5),
                   color = "white",
                   fontface = "bold",
                   size = 3)
print(p)


# 2) Analise de delta de sensaçao e temperatur x velocidade do vento
# Criando um dataframe auxiliar e removendo NAs
cepagri_delta <- cepagri
cepagri_delta$delta <- cepagri_delta$temperatura - cepagri_delta$sensação

# excluindo linhas que sao na
dispersao <- cepagri_delta[!is.na(cepagri_delta$delta) & 
                       !is.na(cepagri_delta$vento) & 
                       !is.na(cepagri_delta$umidade), ]

# matriz correlacao
variaveis <- dispersao[, c("temperatura", "sensação", "vento", "umidade", "delta")]
matriz_cor <- cor(variaveis)
matriz_cor <- round(matriz_cor, 2)

matriz_cor
# Devido a quantidade de pontos no conjunto de dados, a geração do gráfico geom_point
# pode demorar muito, então é possível criar um sample desses dados com 5000 pontos
set.seed(42)
dispersao <- dispersao[sample(nrow(dispersao), 5000), ]

# grafico de dispersao
disp <- ggplot(dispersao, aes(x = vento, y = delta, color = umidade))

# alpha - transparência para o efeito de sobreposição de pontos
disp <- disp + geom_point(alpha = 0.4, size = 1.2)

# linha de tendencia + intervalo de = true para visualizar a correlação
disp <- disp + geom_smooth(method = "lm", formula = y ~ x,
                     color = "red", se = TRUE)

# escala de cor da umidade - baixa laranja, alta azul
disp <- disp + scale_color_continuous(low = "orange", high = "blue")

# rótulos do gráfico
disp <- disp + labs(x = "Velocidade do Vento (km/h)",
              y = "Delta Temperatura - Sensação Térmica (°C)",
              color = "Umidade (%)",
              title = "Relação entre Vento e Diferença Temperatura/Sensação Térmica")
# tema + ajuste
disp <- disp + theme_minimal()
disp <- disp + theme(plot.title = element_text(hjust = 0.5))

print(disp)

# 3) Análise Comparativa da Temperatura Média Mensal nos Períodos Pré-Pandemia, Pandemia e Pós-Pandemia (2018–2023)

# Criando novo dataframe auxiliar
# Filtrando os dados para a analise dos periodos Pre-pandemia, Pandemia e Pos-pandemia
cepagri_pandemia <- cepagri[cepagri$ano >= 2018 & cepagri$ano <= 2023, ]

# Criar periodo para ajudar a montar a tabela
cepagri_pandemia$periodo <- NA
cepagri_pandemia$periodo[cepagri_pandemia$ano >= 2018 & cepagri_pandemia$ano <= 2019] <- "Pre-pandemia"
cepagri_pandemia$periodo[cepagri_pandemia$ano >= 2020 & cepagri_pandemia$ano <= 2021] <- "Pandemia"
cepagri_pandemia$periodo[cepagri_pandemia$ano >= 2022 & cepagri_pandemia$ano <= 2023] <- "Pos-pandemia"

# Definir ordem correta dos períodos
cepagri_pandemia$periodo <- factor(
  cepagri_pandemia$periodo,
  levels = c("Pre-pandemia", "Pandemia", "Pos-pandemia")
)

# Calcular a temperatura média para cada período e mês
temp <- aggregate(temperatura ~ periodo + mes, cepagri_pandemia, mean)
# Renomeando
names(temp) <- c("periodo","mes","temp_media")

# Transformar os dados em tabela para o formato wide
tabela <- reshape(
  temp,
  idvar = "mes",
  timevar = "periodo",
  direction = "wide"
)

# Renomear as colunas para facilitar a leitura
names(tabela) <- c(
  "mes",
  "pre_pandemia",
  "pandemia",
  "pos_pandemia"
)

# Ordenar a tabela pelo mês
tabela <- tabela[order(tabela$mes), ]

# Gerar o gráfico de linhas comparando os períodos
p <- ggplot(tabela, aes(x = mes)) +
  geom_line(aes(y = pre_pandemia, color = "Pre-pandemia"), linewidth = 1.2) +
  geom_line(aes(y = pandemia, color = "Pandemia"), linewidth = 1.2) +
  geom_line(aes(y = pos_pandemia, color = "Pos-pandemia"), linewidth = 1.2) +
  geom_point(aes(y = pre_pandemia, color = "Pre-pandemia")) +
  geom_point(aes(y = pandemia, color = "Pandemia")) +
  geom_point(aes(y = pos_pandemia, color = "Pos-pandemia")) +
  labs(
    title = "Comparação da Temperatura Média",
    subtitle = "Pré-pandemia vs Pandemia vs Pós-pandemia",
    x = "Mês",
    y = "Temperatura média (°C)",
    color = "Período"
  ) +
  theme_minimal()

print(p)

# Exporta tabela no formato ;
write.csv2(tabela, "tabela_pancemia.csv", row.names = FALSE)

# 4) Análise da Temperatura Média com Organização dos Dados por Estações do Ano (2015–2025)
# Criando a coluna de estacao 

# Criando novo dataframe auxiliar
cepagri_estacao <- cepagri

cepagri_estacao$estacao <- NA
cepagri_estacao$estacao[cepagri_estacao$mes %in% c(12,1,2)] <- "Verao"
cepagri_estacao$estacao[cepagri_estacao$mes %in% c(3,4,5)] <- "Outono"
cepagri_estacao$estacao[cepagri_estacao$mes %in% c(6,7,8)] <- "Inverno"
cepagri_estacao$estacao[cepagri_estacao$mes %in% c(9,10,11)] <- "Primavera"

# Faz a media por estacao e ano
temp_estacao <- aggregate(
  temperatura ~ ano + estacao,
  cepagri_estacao,
  mean
)

# Transformar em fator com ordem correta
temp_estacao$estacao <- factor(
  temp_estacao$estacao,
  levels = c("Verao", "Outono", "Inverno", "Primavera")
)

# Gera o grafico Heatmap de temperatura
p <- ggplot(temp_estacao, aes(ano, estacao, fill=temperatura)) +
  geom_tile() +
  labs(
    title="Heatmap da Temperatura Média por Estação",
    x="Ano",
    y="Estação"
  ) +
  scale_fill_gradient(
    low = "blue",
    high = "red",
    name = "Temperatura média (°C)"
  ) +
  theme_minimal()

print(p)

# Monta tabela 
tabela_estacao <- reshape(
  temp_estacao,
  idvar="ano",
  timevar="estacao",
  direction="wide"
)

# Renomeia as colunas da tabela
names(tabela_estacao) <- c(
  "Ano",
  "Verão",
  "Outono",
  "Inverno",
  "Primavera"
)

# Exporta tabela no formato ;
write.csv2(tabela_estacao, "tabela_estacoes.csv", row.names = FALSE)
