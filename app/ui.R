library(dplyr)
library(tidyr)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(maps)

shinyUI(
   fluidPage(
      includeCSS("www/styles.css"),
      navbarPage("Análise exploratória de dados",
                 
         ###########################################
         ######   Contexto na Primeira Aba.   ######
         ###########################################
                 
         tabPanel("Referências do Curso", 
            mainPanel(includeMarkdown("introducao.Rmd"))
          ),
                 
         ############################################################################
         ######   Análise preço de venda combustível no Brasil na Segunda Aba. ######
         ############################################################################
         
         tabPanel("Combustíveis no Brasil",
                  
            sidebarLayout(                  
                     
               mainPanel(
                  div(class="span6",plotOutput("combustivel_linha_brasil")),
                  div(class="span6",plotOutput("combustivel_linha_uf")),
               ),
                     
               sidebarPanel(
                        
                  selectInput(inputId = 'var_produto', 
                              label = 'Escolha uma combustível:',
                              choices = unique(sample_combustiveis_agg_produto_mes$Produto),
                              selected = "GASOLINA"
                  ),
                  
                  selectInput(inputId = 'uf', 
                              label = 'Escolha UF:',
                              choices = unique(sample_combustiveis_agg_uf_produto_mes$UF),
                              selected = "RJ"
                  ),                  
                  
                  selectInput(inputId = 'cor', label = 'Escolha uma cor:',
                              choices = c("purple", "orange", "red"), selected = "red"
                  ),
                  
                  dateRangeInput(inputId = 'x_lim',
                                 label = 'Insira data limite para eixo X',
                                 start = min(sample_combustiveis_agg_produto_mes$Data), 
                                 end = max(sample_combustiveis_agg_produto_mes$Data),
                                 min = min(sample_combustiveis_agg_produto_mes$Data), 
                                 max = max(sample_combustiveis_agg_produto_mes$Data)
                  ),
                  
                  numericRangeInput(inputId = 'y_lim',
                                    label = 'Insira valor mínimo e máximo para eixo y',
                                    value = c(min(sample_combustiveis_agg_produto_mes$Valor_de_Venda), 
                                              max = max(sample_combustiveis_agg_produto_mes$Brent_Real_Barril))
                  )
               )
            )
         )
      )
   )
)