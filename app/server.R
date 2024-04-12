library(tidyverse)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(zoo)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(ggplot2)

shinyServer(function(input, output){
   
   ############################################################################
   ######   Análise preço de venda combustível no Brasil na Segunda Aba. ######
   ############################################################################

   plot_combustiveis_reativo <- eventReactive(c(input$var_produto, 
                                                input$cor),{
      
      ggplot(data = sample_combustiveis_agg_produto_mes %>% 
                dplyr::filter(Produto == input$var_produto),
             aes(x = Data, 
                 y = Valor_de_Venda, 
                 color = "Valor_de_Venda"
             )) + 
         geom_line() +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Valor_de_Venda, color = "Valor_de_Venda")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Valor_de_Venda, color = "Valor_de_Venda")) +                 
         
         geom_line(aes(y = Brent_USD_Barril / 10, color = "Brent_USD_Barril / 10")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Brent_USD_Barril / 10, color = "Brent_USD_Barril / 10")) +         
         
         geom_line(aes(y = Brent_Real_Barril / 50, color = "Brent_Real_Barril / 50")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Brent_Real_Barril / 50, color = "Brent_Real_Barril / 50")) +                  
         
         geom_line(aes(y = PPI, color = "PPI")) + 
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = PPI, color = "PPI")) +         
         
         geom_line(aes(y = Cambio, color = "Cambio")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Cambio, color = "Cambio")) +   
         scale_color_manual(name = "Variáveis", 
                            values = c("Valor_de_Venda" = input$cor, 
                                       "Brent_USD_Barril / 10" = "#d1dc5a", 
                                       "Brent_Real_Barril / 50" = "#e0f7e0", 
                                       "PPI" = "#77f2de", 
                                       "Cambio" = "#6ac5cb"
                            )) +
         
         ylab("# Valor de Venda (R$) vs (Cambio, PPI, Brent)") + 
         xlab("Horizonte Temporal") + 
         labs(title = paste("Evolução temporal no Brasil do produto", input$var_produto, sep = " ")) + 
         theme_bw()            
   })

# ---------------------------------
   
   plot_combustiveis_uf_reativo <- eventReactive(c(input$var_produto, 
                                                input$cor,input$uf),{
      
      ggplot(data = sample_combustiveis_agg_uf_produto_mes %>% 
                dplyr::filter(UF == input$uf & Produto == input$var_produto),
             aes(x = Data, 
                 y = Valor_de_Venda, 
                 color = "Valor_de_Venda"
             )) + 
         geom_line() +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Valor_de_Venda, color = "Valor_de_Venda")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Valor_de_Venda, color = "Valor_de_Venda")) +                 
         
         geom_line(aes(y = Brent_USD_Barril / 10, color = "Brent_USD_Barril / 10")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Brent_USD_Barril / 10, color = "Brent_USD_Barril / 10")) +         
         
         geom_line(aes(y = Brent_Real_Barril / 50, color = "Brent_Real_Barril / 50")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Brent_Real_Barril / 50, color = "Brent_Real_Barril / 50")) +                  
         
         geom_line(aes(y = PPI, color = "PPI")) + 
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = PPI, color = "PPI")) +         
         
         geom_line(aes(y = Cambio, color = "Cambio")) +
         
         geom_point(data = sample_combustiveis_agg_periodo_produto %>%
                       dplyr::filter(Produto == input$var_produto),
                    aes(y = Cambio, color = "Cambio")) +   
         scale_color_manual(name = "Variáveis", 
                            values = c("Valor_de_Venda" = input$cor, 
                                       "Brent_USD_Barril / 10" = "#d1dc5a", 
                                       "Brent_Real_Barril / 50" = "#e0f7e0", 
                                       "PPI" = "#77f2de", 
                                       "Cambio" = "#6ac5cb"
                            )) +
         
         ylab("# Valor de Venda (R$) vs (Cambio, PPI, Brent)") + 
         xlab("Horizonte Temporal") + 
         labs(title = paste("Evolução temporal no Estado do",input$uf, "do produto",input$var_produto, sep = " "),
              subtitle = "Pontos representam Valor de Venda Brasil") + 
         theme_bw()            
   })  
   
    output$combustivel_linha_brasil <- renderPlot({
      plot_combustiveis_reativo()
   })
   
   output$combustivel_linha_uf <- renderPlot({
      plot_combustiveis_uf_reativo()
   })
})       

   
