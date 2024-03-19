# Lista dos pacotes necessários
#, "data.table", "knitr"

package <- c("tidyverse", "ggplot2", "dlookr", "summarytools")

# Veritifica se o pacote está instalado e o instala se for necessário.
is_installed <- package %in% rownames(installed.packages())

if(any(is_installed == FALSE)){
   install.packages(package[!is_installed])
}

# Carregando os Pacotes
invisible(lapply(package, library, character.only = TRUE))

# Removendo variáveis desnecessárias
rm(list=ls())

# Função para carga de um conjunto de dados
extractor_csv2 = function(dados){
   readr::read_csv2(dados, locale = locale(encoding = 'latin1'), show_col_types = FALSE)
}

# Realizando a carga de dados
arquivos <- list.files(pattern = "^ca-", recursive = TRUE)
combustivel_agg <- map_dfr(arquivos, extractor_csv2)
rm("arquivos")

# Análise dos dados

str(combustivel_agg)
head(combustivel_agg)
tail(combustivel_agg)

# Podemos classificar todas as variáveis desse contjunto de dados como:
# Variável              Classificação
# Regiao - Sigla        Qualitativa nominal
# Estado - Sigla        Qualitativa nominal
# Municipio             Qualitativa nominal
# Revenda               Qualitativa nominal
# CNPJ da Revenda       Qualitativa nominal
# Nome da Rua           Qualitativa nominal
# Numero Rua            Qualitativa nominal
# Complemento           Qualitativa nominal
# Bairro                Qualitativa nominal
# Cep                   Qualitativa nominal
# Produto               Qualitativa nominal
# Data da Coleta        Qualitativa nominal
# Valor de Venda        Quantitativa contínua
# Valor de Compra       Quantitativa contínua
# Unidade de Medida     Qualitativa nominal
# Bandeira              Qualitativa nominal

# Análise por Postos
# Calcular quantidade de postos diferentes foram pequisados


# Calcular quantidade de postos diferentes foram pesquisados por produto


combustivel_agg %>% dplyr::filter(Produto == "DIESEL") %>% dplyr::filter(Produto == "DIESEL") %>% table(combustivel_agg$`CNPJ da Revenda`, combustivel_agg$`Regiao - Sigla`)


combustivel_agg %>% dlookr::diagnose()
# 55% dos combustíveis possuem missing no valor da compra

# Análise por Produto

table(combustivel_agg$Produto)
# Os combustíveis são DIESEL, DIESEL S10, DIESEL S50, ETANOL, GASOLINA, GASOLINA ADITIVADA e GNV.


combustivel_agg %>% dplyr::filter(Produto == "DIESEL") %>% dlookr::diagnose()
# 50,6% do Diesel possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(Produto == "DIESEL S10") %>% dlookr::diagnose()
# 71,4% do Diesel S10 possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(Produto == "DIESEL S50") %>% dlookr::diagnose()
# 50,8% do Diesel S50 possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(Produto == "ETANOL") %>% dlookr::diagnose()
# 54,8% do Etanol possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(Produto == "GASOLINA") %>% dlookr::diagnose()
# 49,9% da Gasolina possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(Produto == "GASOLINA ADITIVADA") %>% dlookr::diagnose()
# 100% da Gasolina Aditivada possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(Produto == "GNV") %>% dlookr::diagnose()
# 68,8% do GNV é missing.


# Análise por Região

table(combustivel_agg$`Regiao - Sigla`, combustivel_agg$Produto)
# As regiões são CO, N, NE, S e SE.
# No Centro Oeste 1.809.020 postos foram pesquisados entre 2004 e 2023. 
# No Norte 1.375.118 postos foram pesquisados entre 2004 e 2023.  
# No Nordeste NE 4.385.690 postos foram pesquisados entre 2004 e 2023.
# No Sul 3.990.370 postos foram pesquisados entre 2004 e 2023.
# No Sudeste 11.558.165 postos foram pesquisados entre 2004 e 2023.

combustivel_agg %>% dplyr::select(`Regiao - Sigla`) %>% summarytools::dfSummary()
# A frequência relativa dos postos pesquisados por região são:
# No Centro Oeste foram pesquisados 7,8% dos postos entre 2004 e 2023.
# No Norte foram pesquisados 5,9% dos postos entre 2004 e 2023.
# No Nordeste foram pesquisados 19% dos postos entre 2004 e 2023.
# No Sul foram pesquisados 17,3% dos postos entre 2004 e 2023.
# No Sudeste foram pesquisados 50% dos postos entre 2004 e 2023.

combustivel_agg %>% dplyr::filter(`Regiao - Sigla` == "CO") %>% dlookr::diagnose()
# 62,3% do Centro Oeste possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(`Regiao - Sigla` == "N") %>% dlookr::diagnose()
# 58,6% do Norte possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(`Regiao - Sigla` == "NE") %>% dlookr::diagnose()
# 54,1% do Nordeste possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(`Regiao - Sigla` == "S") %>% dlookr::diagnose()
# 59,9% do Sul possui missing no valor da compra.

combustivel_agg %>% dplyr::filter(`Regiao - Sigla` == "SE") %>% dlookr::diagnose()
# 52% do Sudeste possui missing no valor da compra.

