#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 2
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Anselmo Faria Alvarez Júnior
# - Augusto José Mangini dos Santos
# - Julio Cesar da Silva
# 
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("C:/Users/augus/OneDrive/Área de Trabalho/Cursos/mdc/recuperacao-informacao/trabalho 2") 
options(warn=-1)
source("./ranking_metrics.R")
source("./trabalho2_base.R")

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens 
#----------------------------------------------------------------#
imagens <- read_images(path_plantas)

#----------------------------------------------------------------#
# Obtem classe de cada imagem 
#----------------------------------------------------------------#
nome_classes <- get_classes(path_plantas)

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth(path_plantas, nome_classes, "biloba")
ground_truth_europaea <- get_ground_truth(path_plantas, nome_classes, "europaea")
ground_truth_ilex <- get_ground_truth(path_plantas, nome_classes, "ilex")
ground_truth_monogyna <- get_ground_truth(path_plantas, nome_classes, "monogyna")
ground_truth_regia <- get_ground_truth(path_plantas, nome_classes, "regia")


#----------------------------------------------------------------#
# Questao 1 
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  features <- c(r, g, b)
  return(features)
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  img <- grayscale(img)[,,1,1]
  r1 <- lbp(img,1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(lbp_uniforme)
}


# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  img <- grayscale(img)[,,1,1]
  
  feature <- NULL
  for (i in 0:2) {
    for (j in 0:2) {
      feature <- cbind(feature, momento(img, i, j, central=TRUE))
    }
  }
  return(feature)
}


#----------------------------------------------------------------#
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)

features_t <- t(sapply(imagens, lbp_desc))
rownames(features_t) <- names(imagens)

features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)



#----------------------------------------------------------------#
# Questao 2
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(consulta_biloba, "Biloba")
mostrarImagemColorida(consulta_europaea, "Europaea")
mostrarImagemColorida(consulta_ilex, "Ilex")
mostrarImagemColorida(consulta_monogyna, "Monogyna")
mostrarImagemColorida(consulta_regia, "Regia")


#-----------------------------#
# construindo rankings
generate_ranking <- function(features, query){
  distancia <- dist(features, method = "euclidean")
  distancia <- as.matrix(distancia)
  distancia_interesse <- distancia[,query]
  ranking <- order(distancia_interesse)
  return(ranking)
}
# para cada uma das 5 consultas, construa um ranking com base na cor
ranking_c_biloba <- generate_ranking(features_c, consulta_biloba)
ranking_c_europaea <- generate_ranking(features_c, consulta_europaea)
ranking_c_ilex <- generate_ranking(features_c, consulta_ilex)
ranking_c_monogyna <- generate_ranking(features_c, consulta_monogyna)
ranking_c_regia <- generate_ranking(features_c, consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- generate_ranking(features_t, consulta_biloba)
ranking_t_europaea <- generate_ranking(features_t, consulta_europaea)
ranking_t_ilex <- generate_ranking(features_t, consulta_ilex)
ranking_t_monogyna <- generate_ranking(features_t, consulta_monogyna)
ranking_t_regia <- generate_ranking(features_t, consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- generate_ranking(features_s, consulta_biloba)
ranking_s_europaea <- generate_ranking(features_s, consulta_europaea)
ranking_s_ilex <- generate_ranking(features_s, consulta_ilex)
ranking_s_monogyna <- generate_ranking(features_s, consulta_monogyna)
ranking_s_regia <- generate_ranking(features_s, consulta_regia)

#-----------------------------#
# comparando  rankings

## utilize as funções do arquivo ranking_metrics.R para calcular
# a precisão, revocação, taxa F1 e precisão média nos
# top 5, 10, 15 e 20

analyse_rankings <- function(ranking, ground_truth) {
  top_k <- c(5, 10, 15, 20)
  result <- data.frame(
    top = integer(),
    precisao = numeric(),
    revocacao = numeric(),
    taxa_f1 = numeric(),
    prec_med = numeric()
  )

  for(k in top_k) {
    precisao <- precision(ground_truth, ranking, k)
    revocacao <- recall(ground_truth, ranking, k)
    taxa_f1 <- f1_score(ground_truth, ranking, k)
    prec_med <- ap(ground_truth, ranking, k)

    result <- rbind(result, data.frame(
      top = k,
      precisao = precisao,
      revocacao = revocacao,
      taxa_f1 = taxa_f1,
      prec_med = prec_med
    ))
    cat(paste("Top ", k, ": Precisão = ", precisao, ", Revocação: ", revocacao, 
              ", Taxa F1: ", taxa_f1, ", Precisão Média: ", prec_med, "\n"))
  }
  return(result)
}

# analisando rankings gerados com caracteristicas de cor
results_cor <- list(
  analyse_rankings(ranking_c_biloba, ground_truth_biloba),
  analyse_rankings(ranking_c_europaea, ground_truth_europaea),
  analyse_rankings(ranking_c_ilex, ground_truth_ilex),
  analyse_rankings(ranking_c_monogyna, ground_truth_monogyna),
  analyse_rankings(ranking_c_regia, ground_truth_regia)
); results_cor

# analisando rankings gerados com caracteristicas de textura
results_textura <- list(
  analyse_rankings(ranking_t_biloba, ground_truth_biloba),
  analyse_rankings(ranking_t_europaea, ground_truth_europaea),
  analyse_rankings(ranking_t_ilex, ground_truth_ilex),
  analyse_rankings(ranking_t_monogyna, ground_truth_monogyna),
  analyse_rankings(ranking_t_regia, ground_truth_regia)
); results_textura

# analisando rankings gerados com caracteristicas de forma
results_forma <- list(
  analyse_rankings(ranking_s_biloba, ground_truth_biloba),
  analyse_rankings(ranking_s_europaea, ground_truth_europaea),
  analyse_rankings(ranking_s_ilex, ground_truth_ilex),
  analyse_rankings(ranking_s_monogyna, ground_truth_monogyna),
  analyse_rankings(ranking_s_regia, ground_truth_regia)
); results_forma

#----------------------------------------------------------------#
# Questao 2 - RESPONDA:
# (e)
# Analisando a classe das Regias: Foi usada a imagem 7 como consulta e avaliando
# o resultado, o descritor que retornou o melhor ranking foi o descritor de tex-
# tura, sendo o único com uma revocação de 1 para k = 20, mantendo a precisão
# entre 0.8 e 0.5 de k = 5 a k = 20, com uma precisão média próxima de 0,8. O 
# contexto das imagens pode explicar isso, pois as imagens de Régia possuem uma
# variação grande no número de folhas, variando de 3 a 7, o que explica o descri-
# tor de forma ter o pior desempenho. As cores também possuem uma pequena varia-
# ção nos tons de verde escuro mas menor, portanto o descritor de cor teve o
# desempenho intermediário. A variação em características que podem ser interpre-
# tadas como textura nas folhas é perceptivelmente menor, como a venação por
# exemplo, portanto esse descritor tem o melhor desempenho
#
# (f)

extrair_ap_top10 <- function(ranking) {
  ranking$prec_med[ranking$top == 10]
}

aps_top10_cor <- sapply(results_cor, extrair_ap_top10)
media_ap_top10_cor <- mean(aps_top10_cor); media_ap_top10_cor

aps_top10_textura <- sapply(results_textura, extrair_ap_top10)
media_ap_top10_textura <- mean(aps_top10_textura); media_ap_top10_textura

aps_top10_forma <- sapply(results_forma, extrair_ap_top10)
media_ap_top10_forma <- mean(aps_top10_forma); media_ap_top10_forma

# cor:     0.6690714
# textura: 0.4874127
# forma:   0.3919524
# Os valores obtidos acima mostram que o descritor de cores, considerando a média
# das precisões médias para k = 10 apresenta o melhor resultado. Isso é esperado
# pois analisando as imagens das plantas, a cor entre as categorias parece ser a
# característica com menos variação
#
# (g) dica: use a função generate_df_11_points para gerar o 
# data.frame interpolado para cada descritor. Depois, use a função
# plot_precision_x_recall_11_points_t2 para gerar o gráfico. 
# Exemplo de chamada da função generate_df_11_points:
# df_c_11_points <- generate_df_11_points(list(list(ground_truth_biloba, ranking_c_biloba), list(ground_truth_europaea, ranking_c_europaea)))
# Exemplo de chamada da função plot_precision_x_recall_11_points_t2:
# plot_precision_x_recall_11_points_t2(rbind(df_c_11_points, df_t_11_points, df_s_11_points), c("Cor", "Textura", "Forma"), "Titulo do Gráfico")
#
# 11 pontos para cor
df_c_11_points <- generate_df_11_points(
  list(
    list(ground_truth_biloba, ranking_c_biloba),
    list(ground_truth_europaea, ranking_c_europaea),
    list(ground_truth_ilex, ranking_c_ilex),
    list(ground_truth_monogyna, ranking_c_monogyna),
    list(ground_truth_regia, ranking_c_regia)
    )); df_c_11_points

# 11 pontos para textura
df_t_11_points <- generate_df_11_points(
  list(
    list(ground_truth_biloba, ranking_t_biloba),
    list(ground_truth_europaea, ranking_t_europaea),
    list(ground_truth_ilex, ranking_t_ilex),
    list(ground_truth_monogyna, ranking_t_monogyna),
    list(ground_truth_regia, ranking_t_regia)
  )); df_t_11_points

# 11 pontos para forma
df_s_11_points <- generate_df_11_points(
  list(
    list(ground_truth_biloba, ranking_s_biloba),
    list(ground_truth_europaea, ranking_s_europaea),
    list(ground_truth_ilex, ranking_s_ilex),
    list(ground_truth_monogyna, ranking_s_monogyna),
    list(ground_truth_regia, ranking_s_regia)
  )); df_s_11_points

# gráfico
plot_precision_x_recall_11_points_t2(
  rbind(df_c_11_points, df_t_11_points, df_s_11_points),
  c("Cor", "Textura", "Forma"), 
  " Curva Precisão X Revocação da Precisão Média Interpolada em 11 Pontos"
)

# Analisando o gráfico, no primeiro ponto os 3 descritores possuem a mesma precisão,
# porém a partir do segundo ponto o descritor de cor mantém a precisão maior que
# os outros dois em todos os pontos do gráfico com o aumento da revocação, então
# pode ser considerado o melhor descritor para essa situação
#
#----------------------------------------------------------------#



#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat <- cbind(features_c, features_t, features_s)

# gerar novos rankings
ranking_concat_biloba <- generate_ranking(features_concat, consulta_biloba)
ranking_concat_europaea <- generate_ranking(features_concat, consulta_europaea)
ranking_concat_ilex <- generate_ranking(features_concat, consulta_ilex)
ranking_concat_monogyna <- generate_ranking(features_concat, consulta_monogyna)
ranking_concat_regia <- generate_ranking(features_concat, consulta_regia)
  
# analisando rankings gerados com caracteristicas concatenadas
results_concat <- list(
  analyse_rankings(ranking_concat_biloba, ground_truth_biloba),
  analyse_rankings(ranking_concat_europaea, ground_truth_europaea),
  analyse_rankings(ranking_concat_ilex, ground_truth_ilex),
  analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna),
  analyse_rankings(ranking_concat_regia, ground_truth_regia)
); results_concat


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (d) 
# Os descritores combinados apresentaram resultados piores do que o melhor descritor
# de cada categoria em todos os casos, menos para europaeae onde foi igual ao
# melhor descritor.
#
# (e) 
# É possível observar que o descritor de forma prevaleceu sobre os demais no
# ranking com essa combinação simples dos vetores sem normalização, pois todos os
# resultados replicaram o resultado para o descritor de forma em todas as consultas.
# Isso indica que as caracterísitcas de forma tem uma magnitude dominante nessa
# situação específica.
#
# (f)
df_concat_11_points <- generate_df_11_points(
  list(
    list(ground_truth_biloba, ranking_concat_biloba),
    list(ground_truth_europaea, ranking_concat_europaea),
    list(ground_truth_ilex, ranking_concat_ilex),
    list(ground_truth_monogyna, ranking_concat_monogyna),
    list(ground_truth_regia, ranking_concat_regia)
  ))
# novo grafico
plot_precision_x_recall_11_points_t2(
  rbind(df_c_11_points, df_t_11_points, df_s_11_points, df_concat_11_points),
  c("Cor", "Textura", "Forma", "Concatenado"), 
  " Curva Precisão X Revocação da Precisão Média Interpolada em 11 Pontos"
)
#
# No gráfico, é possível observar que o descritor concatenado se sobrepôes intei-
# ramente ao descritor de forma, portanto as conclusões observadas nos items
# anteriores se mantém iguais.
#
#----------------------------------------------------------------#




#----------------------------------------------------------------#
# Questao 4
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 
dist_hist_biloba <- get_distance_vector(features_c, consulta_biloba)
dist_hist_europaea <- get_distance_vector(features_c, consulta_europaea)
dist_hist_ilex <- get_distance_vector(features_c, consulta_ilex)
dist_hist_monogyna <- get_distance_vector(features_c, consulta_monogyna)
dist_hist_regia <- get_distance_vector(features_c, consulta_regia)
  
# calculando as distancias, descritor:  textura 
dist_text_biloba <- get_distance_vector(features_t, consulta_biloba)
dist_text_europaea <- get_distance_vector(features_t, consulta_europaea)
dist_text_ilex <- get_distance_vector(features_t, consulta_ilex)
dist_text_monogyna <- get_distance_vector(features_t, consulta_monogyna)
dist_text_regia <- get_distance_vector(features_t, consulta_regia)
  
# calculando as distancias, descritor:  forma 
dist_forma_biloba <- get_distance_vector(features_s, consulta_biloba)
dist_forma_europaea <- get_distance_vector(features_s, consulta_europaea)
dist_forma_ilex <- get_distance_vector(features_s, consulta_ilex)
dist_forma_monogyna <- get_distance_vector(features_s, consulta_monogyna)
dist_forma_regia <- get_distance_vector(features_s, consulta_regia) 
  
# calculando e analisando  rankings combmin
r_combmin_biloba <- names(imagens)[combmin(dist_hist_biloba, dist_text_biloba, 
                                           dist_forma_biloba)]
r_combmin_europaea <- names(imagens)[combmin(dist_hist_europaea, dist_text_europaea,
                                            dist_forma_europaea)]
r_combmin_ilex <- names(imagens)[combmin(dist_hist_ilex, dist_text_ilex, 
                                        dist_forma_ilex)]
r_combmin_monogyna <- names(imagens)[combmin(dist_hist_monogyna, dist_text_monogyna, 
                                            dist_forma_monogyna)]
r_combmin_regia <- names(imagens)[combmin(dist_hist_regia, dist_text_regia, 
                                          dist_forma_regia)]

# calculando e analisando  rankings combmax
r_combmax_biloba <- names(imagens)[combmax(dist_hist_biloba, dist_text_biloba, 
                                           dist_forma_biloba)]
r_combmax_europaea <- names(imagens)[combmax(dist_hist_europaea, dist_text_europaea,
                                            dist_forma_europaea)]
r_combmax_ilex <- names(imagens)[combmax(dist_hist_ilex, dist_text_ilex, 
                                        dist_forma_ilex)]
r_combmax_monogyna <- names(imagens)[combmax(dist_hist_monogyna, dist_text_monogyna, 
                                            dist_forma_monogyna)]
r_combmax_regia <- names(imagens)[combmax(dist_hist_regia, dist_text_regia, 
                                          dist_forma_regia)]
  
# calculando e analisando  rankings combsum
r_combsum_biloba <- names(imagens)[combsum(dist_hist_biloba, dist_text_biloba, 
                                           dist_forma_biloba)]
r_combsum_europaea <- names(imagens)[combsum(dist_hist_europaea, dist_text_europaea,
                                            dist_forma_europaea)]
r_combsum_ilex <- names(imagens)[combsum(dist_hist_ilex, dist_text_ilex, 
                                        dist_forma_ilex)]
r_combsum_monogyna <- names(imagens)[combsum(dist_hist_monogyna, dist_text_monogyna, 
                                            dist_forma_monogyna)]
r_combsum_regia <- names(imagens)[combsum(dist_hist_regia, dist_text_regia, 
                                          dist_forma_regia)]
  
# calculando e analisando  rankings borda
r_borda_biloba <- names(imagens)[bordacount(dist_hist_biloba, dist_text_biloba, 
                                           dist_forma_biloba)]
r_borda_europaea <- names(imagens)[bordacount(dist_hist_europaea, dist_text_europaea,
                                            dist_forma_europaea)]
r_borda_ilex <- names(imagens)[bordacount(dist_hist_ilex, dist_text_ilex, 
                                        dist_forma_ilex)]
r_borda_monogyna <- names(imagens)[bordacount(dist_hist_monogyna, dist_text_monogyna, 
                                            dist_forma_monogyna)]
r_borda_regia <- names(imagens)[bordacount(dist_hist_regia, dist_text_regia, 
                                          dist_forma_regia)]

# analyze rankings
results_combmin <- list(
  analyse_rankings(r_combmin_biloba, ground_truth_biloba),
  analyse_rankings(r_combmin_europaea, ground_truth_europaea),
  analyse_rankings(r_combmin_ilex, ground_truth_ilex),
  analyse_rankings(r_combmin_monogyna, ground_truth_monogyna),
  analyse_rankings(r_combmin_regia, ground_truth_regia)
); results_combmin

results_combmax <- list(
  analyse_rankings(r_combmax_biloba, ground_truth_biloba),
  analyse_rankings(r_combmax_europaea, ground_truth_europaea),
  analyse_rankings(r_combmax_ilex, ground_truth_ilex),
  analyse_rankings(r_combmax_monogyna, ground_truth_monogyna),
  analyse_rankings(r_combmax_regia, ground_truth_regia)
); results_combmax

results_combsum <- list(
  analyse_rankings(r_combsum_biloba, ground_truth_biloba),
  analyse_rankings(r_combsum_europaea, ground_truth_europaea),
  analyse_rankings(r_combsum_ilex, ground_truth_ilex),
  analyse_rankings(r_combsum_monogyna, ground_truth_monogyna),
  analyse_rankings(r_combsum_regia, ground_truth_regia)
); results_combsum

results_borda <- list(
  analyse_rankings(r_borda_biloba, ground_truth_biloba),
  analyse_rankings(r_borda_europaea, ground_truth_europaea),
  analyse_rankings(r_borda_ilex, ground_truth_ilex),
  analyse_rankings(r_borda_monogyna, ground_truth_monogyna),
  analyse_rankings(r_borda_regia, ground_truth_regia)
); results_borda

#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) 
# regia -> combmin: 0.2900, combax: 0.7360, combsum: 0.7430, borda: 0.760
# Considerando a mesma consulta utilizada na questão 2, a cosulta para a Régia,
# o método de Borda apresentou o melhor resultado na precisão média para k=20,
# sendo que os outros métodos ficaram próximos, menos o combmin. O método de
# Borda usa a posição dos objetos na agregação e no geral também foi o melhor
# método para as outras plantas.
#
# (j)
df_combmin_11 <- generate_df_11_points(list(
  list(ground_truth_biloba, r_combmin_biloba),
  list(ground_truth_europaea, r_combmin_europaea),
  list(ground_truth_ilex, r_combmin_ilex),
  list(ground_truth_monogyna, r_combmin_monogyna),
  list(ground_truth_regia, r_combmin_regia)
))

df_combmax_11 <- generate_df_11_points(list(
  list(ground_truth_biloba, r_combmax_biloba),
  list(ground_truth_europaea, r_combmax_europaea),
  list(ground_truth_ilex, r_combmax_ilex),
  list(ground_truth_monogyna, r_combmax_monogyna),
  list(ground_truth_regia, r_combmax_regia)
))

df_combsum_11 <- generate_df_11_points(list(
  list(ground_truth_biloba, r_combsum_biloba),
  list(ground_truth_europaea, r_combsum_europaea),
  list(ground_truth_ilex, r_combsum_ilex),
  list(ground_truth_monogyna, r_combsum_monogyna),
  list(ground_truth_regia, r_combsum_regia)
))

df_borda_11 <- generate_df_11_points(list(
  list(ground_truth_biloba, r_borda_biloba),
  list(ground_truth_europaea, r_borda_europaea),
  list(ground_truth_ilex, r_borda_ilex),
  list(ground_truth_monogyna, r_borda_monogyna),
  list(ground_truth_regia, r_borda_regia)
))

plot_precision_x_recall_11_points_t2(
  rbind(df_combmin_11, df_combmax_11, df_combsum_11, df_borda_11),
  c("CombMIN", "CombMAX", "CombSUM", "Borda"),
  "Curva Precisão X Revocação: Métodos de Agregação"
)
# Os métodos de Borda, CombMax e CombSum tiveram desempenho muito próximo na curva
# até a revocação de 0.5, com CombSum e Borda alternando no melhor resultado a 
# partir desse ponto. No útlimo ponto para revocação igual a 1, o método de Borda
# teve uma leve vantagem e foi o que ficou mais próximo de uma precisão igual a
# 0.7 entre todos eles.
# 
# (k)
# Considerando aqui o resultado final em que o método de Borda apresentou melhor
# precisão, constrói-se a tabela abaixo para k=10
#
# extrai AP@10 de cada resultado (posição 2 da lista = top10, coluna prec_med)
ap10 <- function(results) sapply(results, function(r) r$prec_med[2])
tabela_k <- data.frame(
  metodo   = c("Cor", "Textura", "Forma", "Concatenado", "Borda"),
  biloba   = c(ap10(results_cor)[1], ap10(results_textura)[1], ap10(results_forma)[1], 
               ap10(results_concat)[1], ap10(results_borda)[1]),
  europaea = c(ap10(results_cor)[2], ap10(results_textura)[2], ap10(results_forma)[2], 
               ap10(results_concat)[2], ap10(results_borda)[2]),
  ilex     = c(ap10(results_cor)[3], ap10(results_textura)[3], ap10(results_forma)[3], 
               ap10(results_concat)[3], ap10(results_borda)[3]),
  monogyna = c(ap10(results_cor)[4], ap10(results_textura)[4], ap10(results_forma)[4], 
               ap10(results_concat)[4], ap10(results_borda)[4]),
  regia    = c(ap10(results_cor)[5], ap10(results_textura)[5], ap10(results_forma)[5], 
               ap10(results_concat)[5], ap10(results_borda)[5])
)
tabela_k$media <- rowMeans(tabela_k[, -1]); tabela_k
# Na média entre todas as consultas, o método de Borda teve um resultado de 0.6941,
# levemente superior ao melhor descritor individual que foi o de cor com média de
# 0.6690. Portanto, o método de Borda é superior aos métodos individuais e à con-
# catenação simples
#
# (l)
plot_precision_x_recall_11_points_t2(
  rbind(df_c_11_points, df_t_11_points, df_s_11_points,
        df_concat_11_points, df_borda_11),
  c("Cor", "Textura", "Forma", "Concatenado", "Borda"),
  "Curva Precisão X Revocação: Comparação Geral"
)
# Nesse caso, a conclusão é praticamente a mesma da questão anterior pois o méto-
# do de Borda teve precisão igual ou melhor que o método individual de cor para 
# todos os pontos de revocação, menos para o ponto de revocação final igual a 1.
# Portanto, apenas nesse último ponto o método de cor seria melhor.
#
#----------------------------------------------------------------#
