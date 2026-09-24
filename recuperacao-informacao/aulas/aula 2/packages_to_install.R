##################################################################
# Mineração de Dados Complexos -- MDC 2026
# Recuperação de Informação
# Pacotes extras para a Aula 2 - Recuperação de Texto
# 
# 
# 
# Abra este arquivo com o Rstudio e execute cada 
# linha separadamente. Caso encontre algum erro entre 
# em contato com os monitores. Lembre-se de indicar
# aos monitores o seu sistemaa operacional e a versão 
# do R instalada. 
##################################################################

# Instalando o pacote 
install.packages("tm")
# Carregando o pacote
library(tm)
# Mensagem esperada:
# Loading required package: NLP

# !!!! Dependência no Ubuntu libxml2-dev. 
# Instale via no terminal do sistema:
# sudo apt install libxml2-dev
# Em seguida repita o comando install.packages no Rstudio

# Instalando o pacote 
install.packages("dplyr")
# Carregando o pacote
library(dplyr)
# Mensagem esperada:
# 
# Attaching package: ‘dplyr’
# 
# The following objects are masked from ‘package:stats’:
# 
#   filter, lag
# 
# The following objects are masked from ‘package:base’:
# 
#   intersect, setdiff, setequal, union


# Instalando o pacote 
install.packages("udpipe")
# Carregando o pacote
library(udpipe)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
install.packages("tokenizers")
# Carregando o pacote
library(tokenizers)
# Mensagem esperada:
# (Sem mensagem)


# Instalando o pacote 
install.packages("tidytext")
# Carregando o pacote
library(tidytext)
# Mensagem esperada:
# (Sem mensagem)

# Instalando o pacote 
install.packages("tidyverse")
# Carregando o pacote
library(tidyverse)
# Mensagem esperada:
# ── Attaching core tidyverse packages ────────────────────────────────── tidyverse 2.0.0 ──
# ✔ dplyr     1.2.0     ✔ readr     2.2.0
# ✔ forcats   1.0.1     ✔ stringr   1.6.0
# ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
# ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
# ✔ purrr     1.2.1     
# ── Conflicts ──────────────────────────────────────────────────── tidyverse_conflicts() ──
# ✖ dplyr::filter() masks stats::filter()
# ✖ dplyr::lag()    masks stats::lag()

# !!!! Dependência no Ubuntu r-cran-curl. 
# Instale via no terminal do sistema:
# sudo apt install r-cran-curl
# Em seguida repita o comando install.packages no Rstudio
