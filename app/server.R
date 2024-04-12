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
         
   ###########################################
   ######   Salários (Segunda Aba).     ######
   ###########################################
   
   plot_salarios_reativo <- 
      eventReactive(c(input$variaveis_salarios_x,
                      input$variaveis_salarios_y, 
                      input$cor1, 
                      input$x_lim, 
                      input$y_lim),{
                         ggplot(data = sample_combustiveis_agg_n, 
                                aes_string(x = input$variaveis_salarios_x,
                                           y = input$variaveis_salarios_y)) + 
                            geom_point(color = input$cor) + 
                            ggplot2::xlim(input$x_lim) + 
                            ggplot2::ylim(input$y_lim) + 
                            geom_smooth(method = "lm") + 
                            theme_classic()
                         })
   
   
   update_xlim <- 
      eventReactive(c(input$variaveis_salarios_x),{
         if(length(input$variaveis_salarios_x) == 0)
            return(numericRangeInput(inputId = "x_lim",
                                     label = "Insira valor mínimo e máximo para eixo x:", 
                                     value = c(min(sample_combustiveis_agg_n$n), 
                                               max(sample_combustiveis_agg_n$n))))
         updateNumericRangeInput(inputId = "x_lim",
                                 value = c(min(sample_combustiveis_agg_n[,input$variaveis_salarios_x], na.rm = T),
                                           max(sample_combustiveis_agg_n[,input$variaveis_salarios_x], na.rm = T))) 
      })
   
   
   update_ylim <- 
      eventReactive(c(input$variaveis_salarios_y),{
         if(length(input$variaveis_salarios_y) == 0) 
            return(numericRangeInput(inputId = "y_lim",
                                     label = "Insira valor mínimo e máximo para eixo y:", 
                                     value = c(min(sample_combustiveis_agg_n$n), max(sample_combustiveis_agg_n$n))))
         updateNumericRangeInput(inputId = "y_lim", 
                                 value = c(min(sample_combustiveis_agg_n[,input$variaveis_salarios_y], na.rm = T), 
                                           max(sample_combustiveis_agg_n[,input$variaveis_salarios_y], na.rm = T))) 
      })
   
   
   output$salarios_linha <- 
      renderPlot({
         if (((length(input$variaveis_salarios_x) == 0) | (!is.numeric(unlist(sample_combustiveis_agg_n[,input$variaveis_salarios_x][1]))))|
             ((length(input$variaveis_salarios_y) == 0) | (!is.numeric(unlist(salarios[,input$variaveis_salarios_y][1]))))) {
            
            if((!is.numeric(unlist(sample_combustiveis_agg_n[,input$variaveis_salarios_x][1]))) & (length(input$variaveis_salarios_x) != 0)) 
               return(ggplot(sample_combustiveis_agg_n, 
                             aes_string(x=input$variaveis_salarios_x, 
                                        y = input$variaveis_salarios_y)) + 
                         geom_point() + 
                         geom_point(color = input$cor) + 
                         geom_smooth(method = "lm") + 
                         theme_classic())
            
         if((!is.numeric(unlist(sample_combustiveis_agg_n[,input$variaveis_salarios_y][1]))) & (length(input$variaveis_salarios_y) != 0)) 
            return(ggplot(sample_combustiveis_agg_n, 
                          aes_string(x=input$variaveis_salarios_x, 
                                     y = input$variaveis_salarios_y)) + 
                      geom_point() + 
                      geom_point(color = input$cor) + 
                      geom_smooth(method = "lm") + 
                      theme_classic())
            
            else 
               return(ggplot(sample_combustiveis_agg_n, 
                             aes(x=n, y = n)) + 
                         geom_point() + 
                         geom_point(color = input$cor) + 
                         geom_smooth(method = "lm"))
      }
      
      update_ylim()
      
      update_xlim()
      
      plot_salarios_reativo()
   })
})       

   
