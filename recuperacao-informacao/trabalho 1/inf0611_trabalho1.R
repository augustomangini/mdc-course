######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   -   Anselmo Faria Alvarez Júnior                                 #
#   -   Augusto José Mangini dos Santos                              #
#   -   Julio Cesar da Silva                                         #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(tokenizers)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("./ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
setwd("c:/Users/augus/OneDrive/Área de Trabalho/Cursos/mdc/recuperacao-informacao/trabalho 1")


######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)
# Visualizando os documentos (apenas para debuging)
#head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
#head(queries)
# Exemplo de acesso aos tokens de uma consulta
# q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
#head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
#ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
#names(ground_truths)[ground_truths[1,]==1]


# Computando a matriz de termo-documento
term_freq <- document_term_frequencies(docs, term = "word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, k=1.2, b=0.75))
# Visualizando as estatísticas da coleção (apenas para debuging)
#head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  # Dica: você pode acessar a segunda coluna da query a partir de $word ou [["word"]]
  ranking <- get_ranking_by_stats(stat_name = stat_name, docs_stats = stats, tokens_query = query[["word"]] )
  # Visualizando o ranking (apenas para debuging)
  #head(ranking, n = 20)

  # Calculando a precisão
  # Dica: para calcular a precisão, revocação e utilizar a função plot_prec_e_rev,
  # utilize a coluna doc_id do ranking gerado (você pode acessar com $doc_id)
  p <- precision(ground_truth, ranking[["doc_id"]], top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking[["doc_id"]], top)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p,
            "\tRevocação: ", r, "\n"))

  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking = ranking[["doc_id"]], groundtruth = ground_truth, k = top, text = text)
}

# Definindo a consulta 1 
# Dicas para as variáveis consulta1 e n_consulta1:
# Para a variável consulta1, você deve acessar os tokens de uma consulta, conforme
# o exemplo da linha 52 e 53.
# Para a variável n_consulta1, você deve informar o número da consulta. Por exemplo,
# se usar a Query_01 como consulta, n_consulta1 deve receber o valor 1.
consulta1 <- queries[queries$doc_id == "Query_023", ]
n_consulta1 <- 23

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 15, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(
  consulta1,
  ground_truths[n_consulta1, ],
  docs_stats,
  "tf_idf",
  top = 20,
  "Questão 2 - Comparação consulta 23 tf-idf"
)

# Resultados para a consulta 1 e bm25
computa_resultados(
  consulta1,
  ground_truths[n_consulta1, ],
  docs_stats,
  "bm25",
  top = 20,
  "Questão 2 Comparação consulta 23 bm25"
)


# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_029", ]
n_consulta2 <- 29

# Resultados para a consulta 2 e tf_idf
computa_resultados(
  consulta2,
  ground_truths[n_consulta2, ],
  docs_stats,
  "tf_idf",
  top = 20,
  "Questão 2 - Comparação consulta 29 tf-idf"
)

# Resultados para a consulta 2 e bm25
computa_resultados(
  consulta2,
  ground_truths[n_consulta2, ],
  docs_stats,
  "bm25",
  top = 20,
  "Questão 2 Comparação consulta 29 bm25"
)


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
# Para a consulta 23, o tf-idf teve uma precisão de 0,15 e uma revocação
# de 0,6, enquanto o bm25 teve respectivamente 0,2 e 0,8. Se considerarmos
# a precisão como método de avaliação principal, o bm25 retorna o melhor
# ranking (também seria o caso se considerássemos a revocação). 
# No caso da consulta 29, o contrário acontece, onde o tf-idf apresenta
# os melhores dados de precisão e revocação 
# (tf-idf - 0,25 e 0,83, bm25 - 0,2 e 0,6667), portanto é melhor 
# para essa consulta

######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
#head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
#head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc, term = "word")

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, k=1.2, b=0.75))


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_023", ]
n_consulta1_proc <- 23

# Resultados para a consulta 1 e tf_idf
computa_resultados(
  consulta1_proc,
  ground_truths[n_consulta1_proc, ],
  docs_stats_proc,
  "tf_idf",
  top = 20,
  "Questão 3 - Comparação consulta 23 tf-idf"
)

# Resultados para a consulta 1 e bm25
computa_resultados(
  consulta1_proc,
  ground_truths[n_consulta1_proc, ],
  docs_stats_proc,
  "bm25",
  top = 20,
  "Questão 3 - Comparação consulta 23 bm25"
)


# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_029", ]
n_consulta2_proc <- 29

# Resultados para a consulta 2 e tf_idf
computa_resultados(
  consulta2_proc,
  ground_truths[n_consulta2_proc, ],
  docs_stats_proc,
  "tf_idf",
  top = 20,
  "Questão 3 - Comparação consulta 29 tf-idf"
)

# Resultados para a consulta 2 e bm25
computa_resultados(
  consulta2_proc,
  ground_truths[n_consulta2_proc, ],
  docs_stats_proc,
  "bm25",
  top = 20,
  "Questão 3 - Comparação consulta 29 bm25"
)

########################################################
# ii)  Média das precisões médias do sistema todo

calc_media <- function(queries, ground_truths, stats, stat_name, top) {
  ap_values_tfidf <- list()
  
  query_ids <- unique(queries$doc_id)
  nc <- length(query_ids)
  
  for (i in 1:nc) {
    query_atual <- queries[queries$doc_id == query_ids[i], ]
    
    # Gera o ranking para cada query e salva numa lista
    ranking <- get_ranking_by_stats(
      stat_name = stat_name,
      docs_stats = stats,
      tokens_query = query_atual$word
    )
    
    # Calcula precisao media do sisttema
    ap_values_tfidf[i] <- average_precision(
      ground_truths[i, ],
      ranking$doc_id,
      top
    )
    
  }
  
  return(ap_values_tfidf)
}

####################################################################
# TF-IDF
####################################################################
ap_values_tfidf_ss <- calc_media(queries, ground_truths, docs_stats, "tf_idf", top = 20)

# Calcula a média dos valores de precisão média
map_tfidf_com_stopwords <- format(mean(unlist(ap_values_tfidf_ss)), digits = 7)
ap_values_bm25_ss <- calc_media(queries, ground_truths, docs_stats, "bm25", top = 20)
# Calcula a média dos valores de precisão média
map_bm25_com_stopwords <- format(mean(unlist(ap_values_bm25_ss)), digits = 7)

#####################################################################
# BM25
#####################################################################
ap_values_tfidf_cs <- calc_media(queries_proc, ground_truths, docs_stats_proc, "tf_idf", top = 20)

# Calcula a média dos valores de precisão média
map_tfidf_sem_stopwords <- format(mean(unlist(ap_values_tfidf_cs)), digits = 7)
ap_values_bm25_cs <- calc_media(queries_proc, ground_truths, docs_stats_proc, "bm25", top = 20)
# Calcula a média dos valores de precisão média
map_bm25_sem_stopwords <- format(mean(unlist(ap_values_bm25_cs)), digits = 7)

cat("Média das preicsoes tf-idf com stopwords:", map_tfidf_com_stopwords, "\n")
cat("Média das preicsoes tf-idf sem stopwords:", map_tfidf_sem_stopwords, "\n")
cat("Média das preicsoes bm25 com stopwords:", map_bm25_com_stopwords, "\n")
cat("Média das preicsoes bm25 sem stopwords:", map_bm25_sem_stopwords, "\n")

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# i) Olhando para os valores de precisão e revocação para as consultas 23 e 29
# e comparando com os valores com os stopwords mantidos, a única análise
# que teve uma melhora foi para o tf-idf na consulta 23, aumentando a
# precisão para 0,2 e a revocação para 0,8. Comparando o gráfico das duas
# versões, com a remoção dos stopwords é possível observar que a revocação
# atinge um patamar estável maior que anteriormente (~0,32 para 0,6) e com um k
# menor, de 5 para ~4. A precisão se mantém maior com menor K também. Esse
# comportamento da precisão em relação ao K também é parecido para a consulta 29
# antes e depois da remoção das stopwords em tf-idf. Não é possível encontrar
# diferenças tão significativas assim nas outras comparações.
#
# ii) Resultado dos cálculos da média das precisões médias:
# tf-idf com stopwords: 0.4771071
# tf-idf sem stopwords: 0.4986997
# bm25 com stopwords: 0.5713095
# bm25 com stopwords: 0.6037339
# Portanto,o melhor método entre os dois acaba sendo o bm25, visto que sua
# precisão média entre todas as consultas é maior do que do tf-idf tanto com e
# sem stopwords. A remoção dos stopwords se mostra útil pois aumenta a precisão
# em ambos os casos, portanto a melhor opção é usar o método bm25 com remoção de
# stopwords.


######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
#
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
#                              full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
#                               full.names = TRUE)
# file.copy(from=plots.png.paths, to="~/Desktop/")
######################################################################