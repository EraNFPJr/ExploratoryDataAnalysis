library(tidyverse)
library(tidyr)
library(purrr)
library(dlookr)
library(summarytools)
library(readxl)
library(knitr)
library(data.table)
library(ggpubr)
library(corrplot)
library(rcompanion)
library(stargazer)
library(mice)
library(rmarkdown)
library(tinytex)
library(sandwich)
library(magrittr)
library(plm)
library(rvest)
library(shiny)

# setwd('D:/Study/Infnet/DataScience/ExploratoryDataAnalysis/Workspace/GitHub/ExploratoryDataAnalysis')
# 
# # Funções auxiliares
# transform_date_one <- function(data){
#    partes_da_data <- strsplit(data, "[/.]")
#    ano <- as.numeric(sapply(partes_da_data, `[`,3))
#    mes <- as.numeric(sapply(partes_da_data, `[`,2))
#    
#    data_formatada <- format_data(ano, mes)
#    
#    return(data_formatada)
# }
# 
# transform_date_two <- function(data){
#    ano <- as.numeric(substr(data, nchar(data) - 3, nchar(data)))
#    mes <- as.numeric(substr(data, nchar(data) - 5, nchar(data) - 4))
#    
#    data_formatada <- format_data(ano, mes)
#    
#    return(data_formatada)
# }
# 
# transform_date_three <- function(data){
#    ano <- as.numeric(substr(data, nchar(data) - 3, nchar(data)))
#    mes_abreviado <- substr(data, nchar(data) - 7, nchar(data) - 5)
#    mes <- as.integer(match(mes_abreviado, month.abb))
#    
#    data_formatada <- format_data(ano, mes)
#    
#    return(data_formatada)
# }
# 
# format_data <- function(ano_data, mes_data){
#    data_formatada <- as.Date(sprintf("%04d-%02d-01", ano_data, mes_data))
#    return(data_formatada)
# }
# 
# extractor_csv2 = function(dados){
#    readr::read_csv2(dados, locale = locale(encoding = 'UTF-8'), show_col_types = FALSE)
# }
# 
# extractor_csv = function(dados){
#    read.csv(dados, header = FALSE, sep = ";", dec = ",")
# }
# 
# fd <- function(x) {
#    n <-length(x)
#    return((2*IQR(x))/n^(1/3))
# }
# 
# sr <- function(x) {
#    n <-length(x)
#    return((3.49*sd(x))/n^(1/3))
# }
# 
# # Carga dos dados
# arquivos <- list.files(pattern = "^ca-", recursive = TRUE)
# origin_combustiveis_agg <- map_dfr(arquivos, extractor_csv2)
# message("Dados combustíveis carregados dos arquivos CSV.")
# 
# rm(arquivos)
# 
# arquivos <- list.files(pattern = "^CotacoesMoedasPeriodo", recursive = TRUE)
# origin_cambio <- map_dfr(arquivos, extractor_csv)
# message("Dados câmbio carregados dos arquivos CSV.")
# 
# rm(arquivos)
# 
# origin_brent <- read.table("Dataset/Brent/Europe_Brent_Spot_Price_FOB.csv", sep=",", 
#                            header = TRUE)
# message("Dados brent carregados dos arquivos CSV.")
# 
# origin_ppi_agg <- read.table("Dataset/PPI/ppi.csv", sep=";", dec = ",", 
#                              header = TRUE)
# message("Dados ppi carregados dos arquivos CSV.")
# 
# # Normatizando as variáveis da base de combustíveis.
# 
# colnames(origin_combustiveis_agg) <- c("Regiao",
#                                        "UF",
#                                        "Municipio",
#                                        "Revenda",
#                                        "CNPJ_Revenda",
#                                        "Rua",
#                                        "Numero_rua",
#                                        "Complemento",
#                                        "Bairro",
#                                        "CEP",
#                                        "Produto",
#                                        "Data",
#                                        "Valor_de_Venda",
#                                        "Valor_de_Compra",
#                                        "Unidade_de_Medida",
#                                        "Bandeira"
# )
# 
# # Selecionando um conjunto de variáveis da base combustíveis e os produtos.
# sample_combustiveis_agg <- origin_combustiveis_agg[,c(1,2,3,5,11,12,16,13,14)]
# 
# sample_combustiveis_agg <- sample_combustiveis_agg %>% 
#    dplyr::filter(Produto == "GASOLINA" | Produto == "DIESEL S10" | Produto == "DIESEL")
# message("Conjunto de dados combustíveis selecionados.")
# 
# # Normatizando a base de dados brent.
# colnames(origin_brent) <- c("Data", "Brent_USD_Barril")
# message("Base de dados brent normatizado.")
# 
# # Selecionando um conjunto de variáveis e normatizando a base de dados cambio.
# sample_cambio <- origin_cambio[, c(1, 5)]
# colnames(sample_cambio) <- c("Data", "Taxa_Cambio")
# 
# # Padronizando a formatação das datas
# origin_combustiveis_agg$Data <- transform_date_one(origin_combustiveis_agg$Data)
# sample_combustiveis_agg$Data <- transform_date_one(sample_combustiveis_agg$Data)
# origin_brent$Data <- transform_date_three(origin_brent$Data)
# sample_cambio$Data <- transform_date_two(sample_cambio$Data)
# origin_ppi_agg$Data <- transform_date_one(origin_ppi_agg$Data)
# 
# rm(origin_cambio)
# 
# # Cálculo estatísico do câmbio mensal
# calc_cambio <- sample_cambio %>% 
#    dplyr::group_by(Data) %>% 
#    dplyr::summarise(Cambio = mean(Taxa_Cambio),.groups = 'drop')
# 
# # Cálculo estatístico do ppi mensal
# calc_ppi <- origin_ppi_agg %>% 
#    dplyr::group_by(Data, Produto) %>% 
#    dplyr::summarise(PPI = mean(PPI), .groups = 'drop')
# 
# #Merge das bases de dados para criação de uma nova base para análise
# sample_combustiveis_agg <- dplyr::bind_rows(base::merge(sample_combustiveis_agg %>% 
#                                                            dplyr::filter(Produto == "DIESEL" | Produto == "DIESEL S10"), 
#                                                         calc_ppi %>% 
#                                                            dplyr::filter(Produto == "Diesel") %>% 
#                                                            dplyr::select(Data, PPI),
#                                                         by = "Data",
#                                                         all = TRUE),
#                                             base::merge(sample_combustiveis_agg %>% 
#                                                            dplyr::filter(Produto == "GASOLINA"),
#                                                         calc_ppi %>% 
#                                                            dplyr::filter(Produto == "Gasolina") %>% 
#                                                            dplyr::select(Data, PPI),
#                                                         by = "Data",
#                                                         all = TRUE)
# )
# 
# sample_combustiveis_agg <- base::merge(sample_combustiveis_agg, 
#                                        origin_brent, 
#                                        by = "Data", 
#                                        all = TRUE)
# 
# sample_combustiveis_agg <- base::merge(sample_combustiveis_agg, 
#                                        calc_cambio, 
#                                        by = "Data", 
#                                        all = TRUE)
# 
# sample_combustiveis_agg <- sample_combustiveis_agg %>% 
#    dplyr::mutate(Brent_Real_Barril = Brent_USD_Barril * Cambio)
# 
# #Remover mês de setembro de 2020 da base
# sample_combustiveis_agg <- sample_combustiveis_agg %>% dplyr::filter(!Data == "2020-09-01")
# 
# # Criando pontos no período dos dados
# sample_combustiveis_agg_periodo <- sample_combustiveis_agg %>% 
#    dplyr::select(UF, Produto, Data, Valor_de_Venda, Cambio, Brent_USD_Barril, Brent_Real_Barril, PPI) %>% 
#    dplyr::group_by(UF, Produto, Data) %>%
#    dplyr::summarise(
#       Valor_de_Venda = mean(Valor_de_Venda), 
#       Cambio = mean(Cambio),
#       Brent_USD_Barril = mean(Brent_USD_Barril),
#       Brent_Real_Barril = mean(Brent_Real_Barril),
#       PPI = mean(PPI),
#       .groups = 'drop') %>%
#    dplyr::filter(Data %in% c(as.IDate("01-03-2020",
#                                       format = "%d-%m-%Y"), 
#                              as.IDate("01-03-2021",
#                                       format = "%d-%m-%Y"), 
#                              as.IDate("01-03-2022",
#                                       format = "%d-%m-%Y"),
#                              as.IDate("01-03-2023",
#                                       format = "%d-%m-%Y"),
#                              as.IDate("01-03-2024",
#                                       format = "%d-%m-%Y"))
#    )
# 
# # Lista de produtos
# 
# combustiveis <- sample_combustiveis_agg %>% dplyr::select(Produto) %>% base::unique()
# 
# sample_combustiveis_agg_uf_produto_mes <- sample_combustiveis_agg %>% 
#    dplyr::select(UF, Produto, Data, Valor_de_Venda, Cambio, Brent_USD_Barril, Brent_Real_Barril, PPI) %>% 
#    dplyr::group_by(UF, Produto, Data) %>%
#    dplyr::summarise(
#       Valor_de_Venda = mean(Valor_de_Venda), 
#       Cambio = mean(Cambio),
#       Brent_USD_Barril = mean(Brent_USD_Barril),
#       Brent_Real_Barril = mean(Brent_Real_Barril),
#       PPI = mean(PPI),
#       .groups = 'drop')
# 
# sample_combustiveis_agg_produto_mes <- sample_combustiveis_agg_mes %>% 
#    dplyr::select(Produto, Data, Valor_de_Venda, Cambio, Brent_USD_Barril, Brent_Real_Barril, PPI) %>% 
#    dplyr::group_by(Produto, Data) %>% 
#    dplyr::summarise(
#       Valor_de_Venda = mean(Valor_de_Venda),
#       Cambio = mean(Cambio),
#       Brent_USD_Barril = mean(Brent_USD_Barril),
#       Brent_Real_Barril = mean(Brent_Real_Barril),
#       PPI = mean(PPI))

sample_combustiveis_agg_periodo_produto <- sample_combustiveis_agg_periodo %>%
   dplyr::group_by(Produto, Data) %>%
   dplyr::summarise(
      Valor_de_Venda = mean(Valor_de_Venda), 
      Cambio = mean(Cambio),
      Brent_USD_Barril = mean(Brent_USD_Barril),
      Brent_Real_Barril = mean(Brent_Real_Barril),
      PPI = mean(PPI),
      .groups = 'drop')   

sample_combustiveis_agg_n <- sample_combustiveis_agg 
sample_combustiveis_agg_n$n <- seq_len(nrow(sample_combustiveis_agg_n))
