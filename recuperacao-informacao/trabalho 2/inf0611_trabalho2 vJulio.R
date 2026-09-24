#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 2
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - 
# - Julio Cesar da Silva                                       
# -                                        
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

  r1 <- lbp(img, 1)
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
mostrarImagemColorida(consulta_biloba, "Consulta Biloba")
mostrarImagemColorida(consulta_europaea, "Consulta Europaea")
mostrarImagemColorida(consulta_ilex, "Consulta Ilex")
mostrarImagemColorida(consulta_monogyna, "Consulta Monogyna")
mostrarImagemColorida(consulta_regia, "Consulta Regia")
  
#-----------------------------#

generate_ranking <- function(features, query){
  # calculando a distancia dos pontos p e q
  distancia <- dist(features, method = "euclidean")
  distancia <- as.matrix(distancia)
  distancia_interesse <- distancia[,query]
  
  ranking <- order(distancia_interesse)
  # print(ranking)
  return(ranking)
}

# construindo rankings                          
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
  top <- c(5, 10, 15, 20)

  resultados <- data.frame(
    top = integer(),
    precision = numeric(),
    recall = numeric(),
    f1 = numeric(),
    ap = numeric()
  )

  for (t in top) {
    p <- precision(ground_truth, ranking, t)
    r <- recall(ground_truth, ranking, t)
    f1 <- f1_score(ground_truth, ranking, t)
    ap <- ap(ground_truth, ranking, t)

    resultados <- rbind(resultados, data.frame(
      top = t,
      precision = p,
      recall = r,
      f1 = f1,
      ap = ap
    ))

    cat(sprintf("Top %d: Precisão = %.4f, Revocação = %.4f, Taxa F1 = %.4f, Precisão Média = %.4f\n", t, p, r, f1, ap))
  }

  return(resultados)
}

consulta_names <- c("biloba", "europaea", "ilex", "monogyna", "regia")

# analisando rankings gerados com caracteristicas de cor
df_cor <- data.frame(
  consulta = unlist(consulta_names),
  stringsAsFactors = FALSE
)

df_cor$ranking <- list(ranking_c_biloba, ranking_c_europaea, ranking_c_ilex, ranking_c_monogyna, ranking_c_regia)
df_cor$ground_truth <- list(ground_truth_biloba, ground_truth_europaea, ground_truth_ilex, ground_truth_monogyna, ground_truth_regia)
df_cor$resultados <- vector("list", nrow(df_cor))

for (i in seq_len(nrow(df_cor))) {
  cat(sprintf("Ranking Cor - Consulta %s:\n", df_cor$consulta[i]))
  df_cor$resultados[[i]] <- analyse_rankings(
    df_cor$ranking[[i]],
    df_cor$ground_truth[[i]]
  )
  cat("\n")
}


# analisando rankings gerados com caracteristicas de textura
df_textura <- data.frame(
  consulta = unlist(consulta_names),
  stringsAsFactors = FALSE
)

df_textura$ranking <- list(ranking_t_biloba, ranking_t_europaea, ranking_t_ilex, ranking_t_monogyna, ranking_t_regia)
df_textura$ground_truth <- list(ground_truth_biloba, ground_truth_europaea, ground_truth_ilex, ground_truth_monogyna, ground_truth_regia)
df_textura$resultados <- vector("list", nrow(df_textura))

for (i in seq_len(nrow(df_textura))) {
  cat(sprintf("Ranking Textura - Consulta %s:\n", df_textura$consulta[i]))
  df_textura$resultados[[i]] <- analyse_rankings(
    df_textura$ranking[[i]],
    df_textura$ground_truth[[i]]
  )
  cat("\n")
}

# analisando rankings gerados com caracteristicas de forma
df_forma <- data.frame(
  consulta = unlist(consulta_names),
  stringsAsFactors = FALSE
)

df_forma$ranking <- list(ranking_s_biloba, ranking_s_europaea, ranking_s_ilex, ranking_s_monogyna, ranking_s_regia)
df_forma$ground_truth <- list(ground_truth_biloba, ground_truth_europaea, ground_truth_ilex, ground_truth_monogyna, ground_truth_regia)
df_forma$resultados <- vector("list", nrow(df_forma))

for (i in seq_len(nrow(df_forma))) {
  cat(sprintf("Ranking Forma - Consulta %s:\n", df_forma$consulta[i]))
  df_forma$resultados[[i]] <- analyse_rankings(
    df_forma$ranking[[i]],
    df_forma$ground_truth[[i]]
  )
  cat("\n")
}

#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (e) 
# Consulta Classe Biloba
#
# Para a consulta da classe biloba, o descritor que apresentou melhor desempenho 
# foi o de textura, conforme evidenciado pelos maiores valores de precisão, revocação
# e precisão média ao longo dos diferentes níveis de corte do ranking (5, 10, 15 e 20).
#  
# A imagem da classe biloba possui uma cor verde semelhande as outras classes com isso o 
# descritor de cor pode ter tido dificuldades para distinguir a classe biloba das outras.
#
# Sua textura é mais uniforme com os veios quase não visíveis, o que pode ter contribuído 
# para o melhor desempenho do descritor de textura.
#
# O formato da classe é em leque semelhante a outras classes, o que pode ter dificultado a 
# distinção com base no descritor de forma. 
#                                                                             
# (f)  
# Media da Precisão Média (AP) nos top 10 para os rankings de cor
aps_top10_cor <- sapply(df_cor$resultados, function(res) {
  res$ap[res$top == 10]
})

media_ap_top10_cor <- mean(aps_top10_cor); media_ap_top10_cor

# Media da Precisão Média (AP) nos top 10 para os rankings de textura
aps_top10_textura <- sapply(df_textura$resultados, function(res) {
  res$ap[res$top == 10]
})

media_ap_top10_textura <- mean(aps_top10_textura); media_ap_top10_textura


# Media da Precisão Média (AP) nos top 10 para os rankings de forma
aps_top10_forma <- sapply(df_forma$resultados, function(res) {
  res$ap[res$top == 10]
})

media_ap_top10_forma <- mean(aps_top10_forma); media_ap_top10_forma

# cor:     0.3891667  
# textura: 0.4874127
# forma:   0.3919524
#
# Considerando a média das precisões médias (AP) no top 10 para as 5 consultas, 
# o descritor de textura apresentou o melhor desempenho, com valor médio de 0.4874127, 
# superior aos descritores de forma (0.3919524) e cor (0.3891667).
#
# Como a métrica AP leva em conta tanto a relevância quanto a posição dos itens 
# no ranking, um valor mais alto indica que os elementos relevantes foram recuperados 
# com maior frequência nas primeiras posições.

# (g) dica: use a função generate_df_11_points para gerar o 
# data.frame interpolado para cada descritor. Depois, use a função
# plot_precision_x_recall_11_points_t2 para gerar o gráfico. 
# Exemplo de chamada da função generate_df_11_points:
# df_c_11_points <- generate_df_11_points(list(list(ground_truth_biloba, ranking_c_biloba), list(ground_truth_europaea, ranking_c_europaea)))
# Exemplo de chamada da função plot_precision_x_recall_11_points_t2:
# plot_precision_x_recall_11_points_t2(rbind(df_c_11_points, df_t_11_points, df_s_11_points), c("Cor", "Textura", "Forma"), "Titulo do Gráfico") 
#                                         
df_c_11_points <- generate_df_11_points(list(
  list(ground_truth_biloba, ranking_c_biloba), 
  list(ground_truth_europaea, ranking_c_europaea), 
  list(ground_truth_ilex, ranking_c_ilex), 
  list(ground_truth_monogyna, ranking_c_monogyna), 
  list(ground_truth_regia, ranking_c_regia)
))

df_t_11_points <- generate_df_11_points(list(
  list(ground_truth_biloba, ranking_t_biloba), 
  list(ground_truth_europaea, ranking_t_europaea), 
  list(ground_truth_ilex, ranking_t_ilex), 
  list(ground_truth_monogyna, ranking_t_monogyna), 
  list(ground_truth_regia, ranking_t_regia)
))

df_s_11_points <- generate_df_11_points(list(
  list(ground_truth_biloba, ranking_s_biloba), 
  list(ground_truth_europaea, ranking_s_europaea), 
  list(ground_truth_ilex, ranking_s_ilex), 
  list(ground_truth_monogyna, ranking_s_monogyna), 
  list(ground_truth_regia, ranking_s_regia)
))

plot_precision_x_recall_11_points_t2(
  rbind(df_c_11_points, df_t_11_points, df_s_11_points),
  c("Cor", "Textura", "Forma"),
  "Curva Precisão X Revocação da Precisão Média Interpolada em 11 Pontos"
) 

# O melhor descritor que obteve o melhor desempenho no gráfico de precisão x revocação 
# foi o descritor de textura.
# 
#  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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
df_concat <- data.frame(
  consulta = unlist(consulta_names),
  stringsAsFactors = FALSE
)

df_concat$ranking <- list(ranking_concat_biloba, ranking_concat_europaea, ranking_concat_ilex, ranking_concat_monogyna, ranking_concat_regia)
df_concat$ground_truth <- list(ground_truth_biloba, ground_truth_europaea, ground_truth_ilex, ground_truth_monogyna, ground_truth_regia)
df_concat$resultados <- vector("list", nrow(df_concat))

for (i in seq_len(nrow(df_concat))) {
  cat(sprintf("Ranking Concatenado - Consulta %s:\n", df_concat$consulta[i]))
  df_concat$resultados[[i]] <- analyse_rankings(
    df_concat$ranking[[i]],
    df_concat$ground_truth[[i]]
  )
  cat("\n")
}


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (d) 
# 
# 
# (e) 
# 
# 
# 
# 
# (f)
# 
# 
# 
#----------------------------------------------------------------#




#----------------------------------------------------------------#
# Questao 4
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 
dist_hist_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_hist_<to-select> <- <to-do>
  
# calculando as distancias, descritor:  textura 
dist_text_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_text_<to-select> <- <to-do> 
  
# calculando as distancias, descritor:  forma 
dist_forma_<to-select> <- get_distance_vector(<to-do>, <to-do>) 
dist_forma_<to-select> <- <to-do> 
  
# calculando e analisando  rankings combmin
r_combmin_<to-select> <- names(imagens)[combmin(<to-do>, <to-do>, <to-do>)]
r_combmin_<to-select> <- <to-do>

# calculando e analisando  rankings combmax
<to-do>
  
# calculando e analisando  rankings combsum
<to-do>
  
# calculando e analisando  rankings borda
<to-do>

  
analyse_rankings(<to-do>)
analyse_rankings(<to-do>)
  
#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) 
# 
# 
# 
# (j)
# 
# 
# 
# (k)
# 
# 
#
# (l)
# 
#
#
#----------------------------------------------------------------#
