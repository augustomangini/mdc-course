########################################
# Trabalho 2 - INF-0612
# Nome(s):
# Anselmo Faria Alvarez Júnior
# Augusto José Mangini dos Santos
# Julio Cesar da Silva
########################################

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
cepagri$temperatura <- as.numeric(cepagri$temperatura)


# 4) Outliers 
# -> temp negativa -> vento forte e tempestade -> não parece outlier
# sensação de 99 é erro -> todas no mesmo horário]
# como já tem vários NAs nessa coluna, substituir o 99 por NA
cepagri[!is.na(cepagri$sensação) & (cepagri$sensação == 99.9), 5] <- NA


# 9 entradas com temp. e umidade de -7999, podemos excluir
cepagri <- cepagri[cepagri$temperatura != -7999,]


# Umidade do ar a 0% -> acima de 0 o menor valor é 8,8% e é fisicamente
# impossível a umidade do ar chegar a 0% então substituir por NA
cepagri[!is.na(cepagri$umidade) & (cepagri$umidade == 0), 4] <- NA


# 5) Sequência de dados duplicados -> mais de 24 horas de dados duplicados
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

# 6) Adicionando mês e ano para auxiliar nas análises
cepagri$ano <- as.numeric(format(cepagri$horario, "%Y"))
cepagri$mes <- as.numeric(format(cepagri$horario, "%m"))



# Extraindo a data (sem horário) de cada registro
cepagri$data <- as.Date(cepagri$horario)

# Calculando a umidade mínima por dia
umidade_minima_diaria <- aggregate(umidade ~ data, data = cepagri, FUN = min, na.rm = TRUE)

# Contando quantos dias tiveram umidade mínima < 12
dias_umidade_baixa <- nrow(umidade_minima_diaria[umidade_minima_diaria$umidade < 12, ])

print(paste("Dias com umidade mínima abaixo de 12:", dias_umidade_baixa))
