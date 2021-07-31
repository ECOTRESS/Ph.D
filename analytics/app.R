# .libPaths() # Get paths of installed packages
# 
# install.packages("cli")
# require(devtools)
# #install_github('jburkhardt/RAdwords')
# devtools::install_github('selesnow/rgoogleads')
# devtools::install_github("r-lib/gargle")

library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(viridis)
library(DT)
library(gtrendsR)
library(shinyWidgets)
library(googlesheets4)
#library(RAdwords)
#library(rgoogleads)

rm(list = ls())

#setwd(dir = "/Users/usuario/Desktop/Lucas/Expandir_Digital_GoogleAds")

 campanhas = readxl::read_xls("google.xls")
 face =   readxl::read_xls("face.xls")
################################################################
################################################################
###########Google ADS ##########################################

# Ordena dias da semana
level_order = c('Segunda', 'Terça', 'Quarta', 'Quinta', 'Sexta','Sabado', 'Domingo')
# Funcoes de ajuste dataframe
ga_tabela = function(df) {
    df %>%
    filter(`Campaign state` == "enabled") %>%
    select(-c("Keyword",`Quality score`,`Expected clickthrough rate`,
             `Landing page experience`)) %>%
    gather(key = Metricas , value = Valores, - c("Device","Day","Account",`Ad group`,
                                                 "Campaign")) %>%
    mutate(dia_data = weekdays(as.Date(Day))) %>%
    group_by(Day,Account, Campaign, `Ad group`,  Metricas) %>%
    summarise(valores = sum(as.numeric(Valores)),
              dia_data = dia_data
    )  %>%
    # Fine attributes
    mutate(dia_data_pt = case_when(dia_data ==  "Monday" ~ "Segunda",
                                   dia_data ==  "Tuesday" ~ "Terça",
                                   dia_data ==  "Wednesday" ~ "Quarta",
                                   dia_data ==  "Thursday" ~ "Quinta",
                                   dia_data ==  "Friday" ~ "Sexta",
                                   dia_data ==  "Saturday" ~ "Sabado",
                                   dia_data ==  "Sunday" ~ "Domingo"),
           valores_adj = ifelse(valores > 1, 
                                round(valores,1), 
                                round(valores,3))) %>%
    rename(Dia = Day,
           campanha_id = Account,
           Campanha = Campaign,
           `Grupo de anúncios` =  `Ad group`)}

p_tabela = function(df) {
  df %>%
    filter(`Campaign state` == "enabled") %>%
    select(-c(`Quality score`,`Expected clickthrough rate`,
              `Landing page experience`)) %>%
    gather(key = Metricas , value = Valores, - c("Keyword","Device","Day","Account",`Ad group`,
                                                 "Campaign"))  %>%
    mutate(dia_data = weekdays(as.Date(Day))) %>%
    group_by(Day,Account,Keyword, Campaign, `Ad group`,  Metricas) %>%
    summarise(valores = sum(as.numeric(Valores)),
              dia_data = dia_data
    )  %>%
    # Fine attributes
    mutate(dia_data_pt = case_when(dia_data ==  "Monday" ~ "Segunda",
                                   dia_data ==  "Tuesday" ~ "Terça",
                                   dia_data ==  "Wednesday" ~ "Quarta",
                                   dia_data ==  "Thursday" ~ "Quinta",
                                   dia_data ==  "Friday" ~ "Sexta",
                                   dia_data ==  "Saturday" ~ "Sabado",
                                   dia_data ==  "Sunday" ~ "Domingo"),
           valores_adj = ifelse(valores > 1, 
                                round(valores,1), 
                                round(valores,3))) %>%
    rename(Dia = Day,
           campanha_id = Account,
           Campanha = Campaign,
           `Palavra-chave` = Keyword,
           `Grupo de anúncios` =  `Ad group`)}

p_tabela_ind_qual = function(df) {
    df %>%
    filter(`Campaign state` == "enabled") %>%
    mutate(quality = as.numeric(`Quality score`)) %>%
    select(-c(`Quality score`)) %>%
    select( "Day", "Account", "Campaign", "Ad group", "Device", "Keyword", "quality",
            "Clicks", "Cost", "Impressions", "First page CPC","Top of page CPC" ,
            "Conversions", "Expected clickthrough rate", "Landing page experience",
            "All conv." , "Cost / conv.",  "Conv. rate" , "CTR" ,                      
            "Impr. (Abs. Top) %", "Campaign state" ) %>%
    rename(Dia = Day,
           campanha_id = Account,
           Campanha = Campaign,
           `Grupo de anúncios` =  `Ad group`) }
    
#Ajust data
t = ga_tabela(campanhas) # campanha
i = p_tabela(campanhas) # palavra chave
i_ind = p_tabela_ind_qual(campanhas) # p tabela

################################################################
################################################################
###########Facebook ADS ##########################################

f = function(df){
   df %>%
    #filter(`Campaign state` == "enabled") %>%
     select(-c(`Amount Spent`, `CPC (Cost per Link Click)`,
               `CTR (Link Click-Through Rate)`)) %>%
    gather(key = Metricas , value = Valores, - c(Day,`Account Name`,
                                                 `Campaign Name`,
                                                 `Ad Set Name`,
                                                 `Ad Name`)) %>%
    mutate(dia_data = weekdays(as.Date(Day))) %>%
    group_by(Day,
             `Account Name`,
             `Campaign Name`,
             `Ad Set Name`,
             `Ad Name`,
             Metricas) %>%
    summarise(valores = sum(as.numeric(Valores)),
              dia_data = dia_data
    )  %>%
    # Fine attributes
    mutate(dia_data_pt = case_when(dia_data ==  "Monday" ~ "Segunda",
                                   dia_data ==  "Tuesday" ~ "Terça",
                                   dia_data ==  "Wednesday" ~ "Quarta",
                                   dia_data ==  "Thursday" ~ "Quinta",
                                   dia_data ==  "Friday" ~ "Sexta",
                                   dia_data ==  "Saturday" ~ "Sabado",
                                   dia_data ==  "Sunday" ~ "Domingo"),
           valores_adj = ifelse(valores > 1, 
                                round(valores,1), 
                                round(valores,3))) %>%
    rename(Dia = Day,
           campanha_id = `Account Name`,
           Anuncio = `Ad Set Name`,
           Campanha = `Campaign Name` #,
           #`Grupo de anúncios` =  `Ad group`
           )
}

# Ajust data
f_adj = f(face)

# App
ui = dashboardPage(dashboardHeader(
  title = tags$a(href='https://expandirdigital.com.br',
           tags$img(src='WhatsApp Image 2021-07-20 at 11.33.31 AM-2.jpeg',
            height='50',width='50'))),
                   dashboardSidebar(sidebarMenu(
                     menuItem("Google ADS", tabName = "googleads", icon = icon("google")),
                     menuItem("Facebook ADS", tabName = "facebookads", icon = icon("facebook")),
                     menuItem("Google Trends", tabName = "gtrends", icon = icon("googletrends"))
                     )), # Fecha sidebar menu
  dashboardBody(
    setBackgroundColor(
      color = c("#33058D","#E40046" ),
      gradient = "linear",
      direction = "right",
      shinydashboard = T
    ),
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                                  /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                               background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                               background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                               background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                             background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                               background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background:linear-gradient(to right, #000000 0%, #000000 100%)
                                }

                               #  /* body */
                               #  .content-wrapper, .right-side {
                               # background:linear-gradient(to right, #33058D 0%, #E40046 100%);
                               # 
                               #  }

                                '))),
    
    tabItems(
    
      tabItem(
        
        tabName = "googleads",
            
            fluidRow(
             # box(title = "Paramêtros iniciais",
                  selectInput("cliente",
                              label = em("Cliente",style="text-align:center;color:#000000;font-size:100%"),
                              choices= unique(t$campanha_id),
                              selected=as.character(unique(t$campanha_id)[5])),
                  # Define campanha (limited)
                  uiOutput("campanha_botao"),
                  
                  # Define grupo (limited)
                  uiOutput("grupo_botao"),
                  
                  # Define metrica
                  selectInput("type",
                              label = em("Metrica",style="text-align:center;color:#000000;font-size:100%"),
                              choices = unique(t$Metricas),
                              selected=as.character(unique(t$Metricas)[4])),
                  
                  # Define metrica da comparacao
                  selectInput("typecomp",
                              label = em("Metrica a comparar",style="text-align:center;color:#000000;font-size:100%"),
                              choices =  unique(t$Metricas),
                              selected=as.character(unique(t$Metricas)[6])),
      
                  # Define data
                  dateRangeInput('dateRange',
                                 label = em("Data",style="text-align:center;color:#000000;font-size:100%"),
                                 start = as.Date('2021-07-15') ,
                                 end = as.Date('2021-07-30')
                  ),
                  
                  selectInput("semana_dia",#"Dia da semana",
                              label = em("Dia da semana",style="text-align:center;color:#000000;font-size:100%"),
                              choices = c("Segunda","Terça","Quarta", "Quinta","Sexta", "Sabado","Domingo"),
                              selected="Terça")
                  )#, width = 4 # Tamanho da box
              #) # Fecha box 
              ,
              
             # tabBox(
                #title = "Metricas comparativas",
                #id = "tabset1", 
                # tabPanel("Gráfico comparação", plotlyOutput("barChartComparacao")),
                # tabPanel("Mini gráficos", plotlyOutput("minigraficos")),
                # tabPanel("Palavra-chave", plotlyOutput("palavrachave")),
                # tabPanel("Dia da semana", plotlyOutput("diasemana"))#,
        fluidRow(
             plotlyOutput("barChartComparacao"),
             plotlyOutput("minigraficos"),
             plotlyOutput("palavrachave", height = 900),
             plotlyOutput("diasemana") #,
        )
               # height = 12,width = 8
              #  ) # Fecha tabBox
              ,
              
              # Palavra chave (limited)
              #box(uiOutput("palavra_botao")),
              
              # tabBox(
              #   title = "palavra chave",
              #   id = "tabset1", 
              #   tabPanel("Palavra-chave", plotlyOutput("palavrachave")),
              #   height = '30%',
              #   width = 8
              # ) # Fecha tabBox
          
             # box(
                #DTOutput('table'),
                DTOutput("table")#,
             #  width = 12 )    
              #) # Fecha Fluid row
            ), # Fecha itemtab
   
     # Facebook
    tabItem(
      
      tabName = "facebookads",
            
            fluidRow(
              #box(title = "Paramêtros iniciais",
                  selectInput("cliente_f",
                              label = em("Cliente",style="text-align:center;color:#000000;font-size:100%"),
                              choices= unique(f_adj$campanha_id),
                              selected=as.character(unique(f_adj$campanha_id)[2])),
                  
                  # Define campanha (limited)
                  uiOutput("campanha_botao_f"),
                  
                  # Define grupo (limited)
                  #uiOutput("grupo_botao_f"),
                  
                  selectInput("grupo_botao_f",
                              label = em("Grupo",style="text-align:center;color:#000000;font-size:100%"),
                              choices = c(as.character(f_adj$Anuncio)#[as.character(f_adj$Anuncio) == input$campanha_botao_f]
                              ),
                              selected =  as.character(f_adj$Anuncio[1])),
                  
                  #uiOutput("ad_botao_f"),
                  
                  # Define metrica
                  selectInput("type_f",
                              label = em("Metrica",style="text-align:center;color:#000000;font-size:100%"),
                              choices = unique(f_adj$Metricas),
                              selected= as.character(unique(f_adj$Metricas)[7])),
                  
                  # Define metrica da comparacao
                  selectInput("typecomp_f",
                              label = em("Metrica a comparar",style="text-align:center;color:#000000;font-size:100%"),
                              choices =  unique(f_adj$Metricas),
                              selected= as.character(unique(f_adj$Metricas)[9])),
                  
                  # Define data
                  dateRangeInput('dateRange_f',
                                 label = em("Data",style="text-align:center;color:#000000;font-size:100%"),
                                 start = as.Date('2021-07-01') ,
                                 end = as.Date('2021-07-30')
                  ) #,width = 4 # Tamanho da box
             # ) # Fecha box 
              ,
              
             # tabBox(
              #  title = "Metricas comparativas",
               # id = "tabset1", 
                plotlyOutput("barChartComparacao_f"),
                plotlyOutput("minigraficos_f"),
                plotlyOutput("barChartAnuncio_f")#,
               #,
                #height = '30%',
                #width = 8
             # ) # Fecha tabBox
              ,
              
              # Palavra chave (limited)
              #box(uiOutput("palavra_botao")),
              
              # tabBox(
              #   title = "palavra chave",
              #   id = "tabset1", 
              #   tabPanel("Palavra-chave", plotlyOutput("palavrachave")),
              #   height = '30%',
              #   width = 8
              # ) # Fecha tabBox
              
             # box(DTOutput('table'),
              #    width = 12)    
            ) # Fecha Fluid row
    ) # Fecha tab item
    ) # Fecha tab itens 
  ) # Fecha body
) # Fecha page

server = function(input, output) {
  # Funcoes de plotagem

  plot_fun_comparacao = function(df){
    df %>%
      filter((Metricas == input$type | Metricas == input$typecomp) &
               campanha_id == input$cliente & Campanha == input$campanha_botao &
               `Grupo de anúncios` == input$grupo_botao) %>%
      #Add date column
      mutate(data_data = as.Date(Dia)) %>%
      #Filtro de data
      filter(data_data >= input$dateRange[1] & data_data <= input$dateRange[2]) %>%
      ggplot() + aes(
        x = Dia,
        y = valores_adj, fill = Metricas,
        label= dia_data_pt) +
      geom_col(position = "dodge") +
      xlab("") + ylab("Valores") +
      ggtitle("Metricas") + 
      # Add a second axis and specify its features
      #sec.axis = sec_axis( trans=~.*100, name="Second Axis"))
      scale_fill_viridis(discrete = TRUE) +
      # guides(fill=guide_legend(title="New Legend Title")) +
      theme_bw() +
      #facet_wrap(~ `Grupo de anúncios`) +
      theme(#legend.position = c(0.1,0.1),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = c(0.8,0.8))
    
  }
  
  plot_mini = function(df){
    df %>%
      filter(campanha_id == input$cliente  &
            `Grupo de anúncios` == input$grupo_botao 
             #  Campanha == input$campanha
      ) %>%
      # Add date column
      mutate(data_data = as.Date(Dia)) %>%
      # Filtro de data
      filter(data_data >= input$dateRange[1] & data_data <= input$dateRange[2] &
               Campanha == input$campanha_botao) %>%
      # )) %>%
      ggplot() + aes(
        x = Dia,
        y = valores_adj,
        fill = Metricas,
        label= dia_data_pt) +
      geom_col(position = "dodge") +
      xlab("") + ylab("Valores") +
      scale_fill_viridis(discrete = TRUE) +
      ggtitle("Todas as metricas") +
      facet_wrap(~Metricas, scales = "free_y") +
      # guides(fill=guide_legend(title="New Legend Title")) +
      theme_bw(base_size = 8) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            legend.title = element_blank()
      )
    
  }
  
  plot_palavra_chave = function(df){
    df %>%
      mutate(data_data = as.Date(Dia)) %>%
      filter((Metricas == input$type | Metricas == input$typecomp) &
               data_data >= input$dateRange[1] & 
               data_data <= input$dateRange[2] &
              # `Palavra-chave` == input$palavra_botao &
               campanha_id == input$cliente & 
               Campanha == input$campanha_botao
        #&
             # Limitar por grupo!
      ) %>%
      ggplot() + aes(
        x = Dia,
        y = valores_adj,
        fill = Metricas,
        label= dia_data_pt
      ) +  geom_col(position = "dodge") +
      
      xlab("") + ylab("Valores") +
      #ylim(c(0,10)) +
      scale_fill_viridis(discrete = TRUE) +
      ggtitle("Nivel de Palavra chave: métricas") + 
      facet_wrap(~`Palavra-chave`) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            legend.title = element_blank()
      )
    
  }
  
  plot_dia_semana = function(df){
    df %>%
      filter((Metricas == input$type | Metricas == input$typecomp) &
               campanha_id == input$cliente & Campanha == input$campanha_botao &
               `Grupo de anúncios` == input$grupo_botao &
                dia_data_pt == input$semana_dia
               ) %>%
      # Add date column
      mutate(data_data = as.Date(Dia)) %>%
      # Filtro de data
      filter(data_data >= input$dateRange[1] & data_data <= input$dateRange[2]) %>%
      # )) %>%
      ggplot() + aes(#x = factor(dia_data_pt, level = level_order),
       x = Dia,
         y = valores_adj, fill = Metricas,
        label= dia_data_pt) +
      geom_col(position = "dodge") +
      xlab("") + ylab("Valores") +
      scale_fill_viridis(discrete = TRUE) +
      # ggtitle("Metricas dia da semana") +
      # facet_wrap(~Campanha, scales = "free_y") +
      # guides(fill=guide_legend(title="New Legend Title")) +
      theme_bw() +
      theme(legend.position = "none",
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  }

  # Tabela reativa
  plot_table = function(df){
     df %>%
      mutate(data_data = as.Date(Dia)) %>%
      #Filtro de data
      filter(data_data >= input$dateRange[1] & data_data <= input$dateRange[2]) %>%
      filter( #(Metricas == input$type | Metricas == input$typecomp) &
        campanha_id == input$cliente & Campanha == input$campanha_botao &
          `Grupo de anúncios` == input$grupo_botao) 
}
  
 
  ######################################
  ######################################
  ######################################
  # Funcoes de plotagem face
  plot_comparacao_face = function(df){

    df %>%
      filter((Metricas == input$type_f | Metricas == input$typecomp_f) &
               campanha_id == input$cliente_f &
               Campanha == input$campanha_botao_f &
               Anuncio == input$grupo_botao_f #&
              # Ad.Name == input$ad_botao_f
             ) %>%
      #Add date column
      mutate(data_data = as.Date(Dia)) %>%
      #Filtro de data
      filter(data_data >= input$dateRange_f[1] & data_data <= input$dateRange_f[2]) %>%
      ggplot() + aes(
        x = Dia,
        y = valores_adj, fill = Metricas,
        label= dia_data_pt) +
      geom_col(position = "dodge") +
      xlab("") + ylab("Valores") +
      scale_fill_viridis(discrete = TRUE) +
      # guides(fill=guide_legend(title="New Legend Title")) +
      theme_bw() +
      #facet_wrap(~ `Grupo de anúncios`) +
      theme(#legend.position = c(0.1,0.1),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = c(0.8,0.8))

  }
  
  
  # Ver diferentes anuncios
  plot_anuncio_face = function(df){
    
    df %>%
      filter((Metricas == input$type_f | Metricas == input$typecomp_f) &
               campanha_id == input$cliente_f &
               Campanha == input$campanha_botao_f #& 
               #Anuncio == input$grupo_botao_f
              # Campanha == input$campanha_botao_f & 
              # Anuncio == input$grupo_botao_f &
              # Ad.Name == input$ad_botao_f
             ) %>%
      #Add date column
      mutate(data_data = as.Date(Dia)) %>%
      #Filtro de data
      filter(data_data >= input$dateRange_f[1] & data_data <= input$dateRange_f[2]) %>%
      ggplot() + aes(
        x = Dia,
        y = valores_adj, fill = Metricas,
        label= dia_data_pt) +
      geom_col(position = "dodge") +
      xlab("") + ylab("Valores") +
      scale_fill_viridis(discrete = TRUE) +
      # guides(fill=guide_legend(title="New Legend Title")) +
      theme_bw() +
      facet_wrap(~Anuncio) +
      theme(#legend.position = c(0.1,0.1),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        legend.position = c(0.8,0.8))
    
  }
  
  # Mini face
  plot_mini_face =  function(df){
    df %>%
      filter(
        #(Metricas == input$type_f | Metricas == input$typecomp_f) &
               campanha_id == input$cliente_f &
               Campanha == input$campanha_botao_f #& 
             # Anuncio == input$grupo_botao_f
             # Campanha == input$campanha_botao_f & 
             # Anuncio == input$grupo_botao_f &
             # Ad.Name == input$ad_botao_f
      ) %>%
      #Add date column
      mutate(data_data = as.Date(Dia)) %>%
      #Filtro de data
      filter(data_data >= input$dateRange_f[1] & data_data <= input$dateRange_f[2]) %>%
      ggplot() + aes(
        x = Dia,
        y = valores_adj, fill = Metricas,
        label= dia_data_pt) +
      geom_col(position = "dodge") +
      xlab("") + ylab("Valores") +
      scale_fill_viridis(discrete = TRUE) +
      # guides(fill=guide_legend(title="New Legend Title")) +
      theme_bw() +
      facet_wrap(~Metricas) +
      theme(#legend.position = c(0.1,0.1),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)#,
        #legend.position = c(0.8,0.8)
        )
    
  }
 
  # Google ads ################### ################### ###################
  ################### ################### ###################
  # Botao 1: campanha
  output$campanha_botao = renderUI({
    selectInput("campanha_botao",# "Campanha",
                label = em("Campanha",style="text-align:center;color:#000000;font-size:100%"),
                choices = c(as.character(t$Campanha)[as.character(t$campanha_id) == input$cliente]),
                selected =  as.character(t$Campanha)[1]
    )
  })
  
  # Botao 2: grupo
  output$grupo_botao =  renderUI({
    selectInput("grupo_botao",
                label = em("Grupo",style="text-align:center;color:#000000;font-size:100%"),
                choices = c(as.character(t$`Grupo de anúncios`)[as.character(t$campanha_id) == input$cliente]),
                selected =  as.character(t$`Grupo de anúncios`)[1])
  })
  
  # Botao 3: palavra chave
  # output$palavra_botao =
  #   renderUI({
  # selectInput("palavra_chave",
  #             label = em("Palavra chave",style="text-align:center;color:#000000;font-size:100%"),
  #             #choices = unique(i$`Palavra-chave`),
  #             choices = c(as.character(i$Campanha)[as.character(i$campanha_id) == input$cliente]),
  #             selected = as.character(unique(i$Campanha)[1]))
  #   })
  
  # Grafico 1: evolucao diaria
  output$barChartComparacao = renderPlotly({
    p1 = plot_fun_comparacao(t)
    ggplotly(p1) 
  })
  
  # Grafico 2: mini graficos evolucao diaria
  output$minigraficos = renderPlotly({
    p2 = plot_mini(t)
    ggplotly(p2)
  })
  
  # Grafico 3: palavra chave
  output$palavrachave = renderPlotly({

    p3 = plot_palavra_chave(i)
    ggplotly(p3)
  })
  
  # Grafico 7: evolucao nivel anuncio
  output$diasemana = renderPlotly({
    p7 = plot_dia_semana(t)
    ggplotly(p7)
  })

  # plot table
  
  #p8 = plot_table(i_ind)
  
  output$table = renderDT(i_ind)
    
  #   DT::renderDataTable({
  #   DT::datatable(i_ind[input$cliente,])
  # })
    
    # df %>%
    # mutate(data_data = as.Date(Dia)) %>%
    # #Filtro de data
    # filter(data_data >= input$dateRange[1] & data_data <= input$dateRange[2]) %>%
    # filter( #(Metricas == input$type | Metricas == input$typecomp) &
    #   campanha_id == input$cliente & Campanha == input$campanha_botao &
    #     `Grupo de anúncios` == input$grupo_botao) 
    # 
    
    
    
    
    
    #renderDT(i_ind)
   
    #,options = list(lengthChange = FALSE)
       #  renderDT(p8))
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  
  # Facebook
  
  # Botao 1: campanha_botao
  output$campanha_botao_f =  renderUI({
    selectInput("campanha_botao_f",
                label = em("Campanha",style="text-align:center;color:#000000;font-size:100%"),
                choices = c(as.character(f_adj$Campanha)[as.character(f_adj$campanha_id) == input$cliente_f]),
                selected =  as.character(f_adj$Campanha[1]))
  })
  #
  # # # Botao 2: anuncio
  # output$grupo_botao_f =  renderUI({
  #   selectInput("grupo_botao_f",
  #               label = em("Anuncio",style="text-align:center;color:#000000;font-size:100%"),
  #               choices = c(as.character(f_adj$Anuncio)[as.character(f_adj$Anuncio) == input$campanha_botao_f]),
  #               selected =  as.character(f_adj$Anuncio[1]))
  # })
  #
  # # Botao 3: ad.name
  # output$ad_botao_f = renderUI({
  #   selectInput("ad_botao_f",
  #               label = em("Nome do anuncio",style="text-align:center;color:#000000;font-size:100%"),
  #               choices = c(as.character(f_adj$Ad.Name)[as.character(f_adj$Ad.Name) == input$cliente_f]),
  #               selected =  as.character(f_adj$Ad.Name[1])
  #   )
  # })


  # Grafico 4: evolucao diaria
  output$barChartComparacao_f = renderPlotly({

    p4 = plot_comparacao_face(f_adj)
    ggplotly(p4)
  })


  # Grafico 5: evolucao nivel anuncio
  output$barChartAnuncio_f = renderPlotly({
  p5 = plot_anuncio_face(f_adj)
  ggplotly(p5)
  })

  # Grafico 6: evolucao nivel anuncio
  output$minigraficos_f = renderPlotly({
    p6 = plot_mini_face(f_adj)
    ggplotly(p6)
  })
  
  
  
  
  
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  
}
shinyApp(ui = ui, server = server)
