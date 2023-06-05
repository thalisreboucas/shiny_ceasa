library(pacman)
pacman::p_load(shiny,shinydashboard,
               bs4Dash,bs4cards,waiter,fresh,
               thematic,shinythemes,DT,zoo,plotly, # Making dynamic graphs
               modeltime, # Times model classics 
               tidyverse, # All tidy for manipulation
               timetk, # Making a tables and graph's of timeseries
               lubridate, # Working if date
               easystats, # making a spells on the datas.
               imputeTS,
               readxl)

thematic_shiny()

########################################## trocar infos -- toast options ############################
toastOpts <- list(
  autohide = TRUE,
  icon = "fas fa-home",
  close = FALSE,
  position = "bottomRight"
)



######### color statuses #######
statusColors <- c(
  "gray-dark",
  "gray",
  "secondary",
  "navy",
  "indigo",
  "purple",
  "primary",
  "lightblue",
  "info",
  "success",
  "olive",
  "teal",
  "lime",
  "warning",
  "orange",
  "danger",
  "fuchsia",
  "maroon",
  "pink",
  "white"
)
##################### my theme ###########
mytheme <- create_theme(
  bs4dash_vars(
    navbar_light_hover_color = "#3BA368"
  ),
  bs4dash_sidebar_light(
    hover_color = "#3BA368"
  ),
  bs4dash_status(
    primary = "#3BA368", danger = "#D10003"
  ),
  bs4dash_vars()
)

# actual_price_tab  ###################
actual_price_tab <- tabItem(
  tabName = "tab_prices",
  fluidRow(
    box(width = 4,
        status = "primary",
        title = "Filtro de data para os itens",
        solidHeader = TRUE, 
    dateRangeInput('dateRange',
                   label = '',
                   language = "pt",
                   min = min(data$date),
                   max = max(data$date),
                   start = max(data$date) , 
                   end =  "2023-01-01 UTC" ,
                   format = "dd/mm/yyyy" ,
                   separator = "Até" )),
    tabBox(
      title = "Tabela de Preços da Ceasa", 
      closable = FALSE, 
      width = 12,
      status = "primary", 
      solidHeader = TRUE, 
      collapsible = FALSE,
      type = "tabs",
      selected = "Preço diário",
      tabPanel(
        "Média Mensal",
      DT::dataTableOutput("tbl",width = '1200px')
    ) ,
    tabPanel(
      "Preço diário",
      DT::dataTableOutput("tbl_1",width = '1200px')
   ))) 

  )


# analises_tab ####################
 analises_tab <- tabItem(
  tabName = "analises_tab",
  fluidRow(
    
    box(
      closable = FALSE, 
      width = 3,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
    selectInput(
      inputId = "item",
      label = "Escolha o item:",
      c(
        "ABACATE"=1,
        "ABACAXI"=2,
        "ABOBORA"=3,
        "ABOBRINHA"=4,
        "ALFACE"=5,
        "ALHO"=6,
        "BANANA NANICA"=7,
        "BANANA PRATA"=8,
        "BATATA"=9,
        "BATATA DOCE"=10,
        "BERINJELA"=11,
        "BETERRABA"=12,
        "BROCOLO"=13,
        "CARA"=14,
        "CEBOLA"=15,
        "CENOURA"=16,
        "CHUCHU"=17,
        "COCO VERDE"=18,
        "COUVE"=19,
        "COUVE-FLOR"=20,
        "GOIABA"=21,
        "INHAME"=22,
        "JILO"=23,
        "LARANJA PERA"=24,
        "LIMAO TAHITI"=25,
        "MACA"=26,
        "MAMAO FORMOSA"=27,
        "MAMAO HAWAY"=28,
        "MANDIOCA"=29,
        "MANDIOQUINHA"=30,
        "MANGA"=31,
        "MARACUJA AZEDO"=32,
        "MELANCIA"=33,
        "MELAO AMARELO"=34,
        "MILHO VERDE"=35,
        "MORANGO"=36,
        "OVOS"=37,
        "PEPINO"=38,
        "PERA IMPORTADA"=39,
        "PIMENTAO VERDE"=40,
        "QUIABO"=41,
        "REPOLHO"=42,
        "TANGERINA"=43,
        "TOMATE"=44,
        "UVA ITALIA"=45,
        "UVA NIAGARA"=46,
        "VAGEM"=47)),
    selectInput(
      multiple = TRUE,
      inputId = "year_violin",
      label = "Escolha o ano:",
      choices = c(2015,2016,2017,2018,2019,2020,2021,2022,2023),
      selected = 2023)
    )
    ,
    box(
      title = "Tabela com medidas de escala e dispersão", 
      closable = FALSE, 
      width = 9,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
      tableOutput("tbl_eda_1")
    ),
    
    tabBox(
      title = "Gráfico da distribuição de preços (Boxplot)", 
      closable = FALSE, 
      width = 12,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
      type = "tabs",
      selected = "Ano",
      tabPanel("Ano",plotly::plotlyOutput("plot_violin")),
      tabPanel("Mês",plotly::plotlyOutput("plot_violin_month")),
      tabPanel("Dia da Semana",plotly::plotlyOutput("plot_violin_week")),
      tabPanel("Semanas",plotly::plotlyOutput("plot_violin_weeks")),
      tabPanel("Quarto de ano",plotly::plotlyOutput("plot_violin_quarter"))
      )
    
   ),
  fluidRow(
    tabBox(
      title = "Gráfico de Serie temporal", 
      width = 12,
      status = "primary", 
      closable = FALSE,
      maximizable = TRUE, 
      solidHeader = TRUE,
      collapsible = TRUE,
      type = "tabs",
      selected = "Gráfico de Tendência",
      tabPanel("Gráfico de Tendência",plotly::plotlyOutput("plot_eda")),
      tabPanel("Grafico de Anomalia",plotly::plotlyOutput("plot_an")),
      tabPanel("Gráfico do Restante",plotly::plotlyOutput("plot_stl"))
      
    )
  ),
  
  fluidRow(
   tabBox(
      title = "Grafico de diagnósticos de Lags", 
      width = 12,
      status = "primary", 
      closable = FALSE,
      maximizable = TRUE, 
      solidHeader = TRUE, 
      collapsible = TRUE,
      type = "tabs",
      selected = "Gráfico de Correlação ACF",
      tabPanel("Gráfico de Correlação ACF", plotly::plotlyOutput("plot_acf")),
      tabPanel("Gráfico de Correlação PACF", plotly::plotlyOutput("plot_pacf"))
    )
 )
)


# forcast_model_tab ---- #########################
forcast_model_tab <- tabItem(
  tabName = "forecast_tab",
  fluidRow(
    
  )
)

# theory_tab ---- 
theory_tab <- tabItem(
  tabName = "theory_tab",
  fluidRow(
    column(
      width = 6,
      tabBox(
        title = "Modelos Utilizados",
        elevation = 2,
        id = "tabcard1",
        width = 12,
        collapsible = FALSE, 
        closable = FALSE,
        type = "tabs",
        status = "primary",
        solidHeader = TRUE,
        selected = "Prophet",
        tabPanel(
          "Prophet",
          "O modelo Prophet é um modelo de séries temporais desenvolvido pelo Facebook, projetado para fazer previsões de séries temporais com tendência não linear, sazonalidade e feriados. Ele é baseado em uma abordagem de decomposição aditiva, onde a série temporal é dividida em três componentes principais: tendência, sazonalidade e feriados. O modelo Prophet usa uma regressão não linear para modelar a tendência e um modelo de regressão aditiva para modelar sazonalidade e feriados. Ele utiliza técnicas de aprendizado de máquina para selecionar automaticamente os melhores parâmetros para o modelo.

O modelo Prophet é especialmente útil para previsões de curto e médio prazo de séries temporais que contêm sazonalidade e efeitos de feriados. Ele requer pouca ou nenhuma intervenção manual para ajuste de parâmetros e pode lidar com lacunas na série temporal. Além disso, ele fornece intervalos de incerteza para as previsões, o que significa que é possível avaliar o grau de confiança das previsões.

Este modelo é popular na indústria devido à sua facilidade de implementação e desempenho sólido em uma ampla gama de aplicações, desde previsões de vendas até previsões de produção e demanda de energia. No entanto, ele não é recomendado para séries temporais extremamente curtas ou irregularmente espaçadas. 
Fonte :ChatGPT"),
        tabPanel(
          "XGBoost",
          "O XGBoost é um modelo de aprendizado de máquina muito popular que usa a técnica de boosting para produzir modelos altamente precisos e escaláveis. O modelo é muito útil para previsão de séries temporais, pois é capaz de lidar com dados não lineares e com diferentes escalas de tempo.

Existem algumas considerações importantes ao utilizar o XGBoost na previsão de séries temporais, como a escolha das variáveis usadas para treinar o modelo e a definição correta dos parâmetros de configuração. Aqui estão alguns passos importantes e recomendações para a construção de um modelo XGBoost para previsão de séries temporais:

Preparação dos dados: antes de treinar o modelo, é importante preparar os dados da série temporal. Isso pode incluir a limpeza de dados ausentes ou inconsistentes, a normalização de dados e a agregação de séries temporais em janelas de tempo.

Escolha de variáveis: para prever uma série temporal, é importante escolher as variáveis que podem afetar o resultado. Isso pode incluir variáveis históricas, variáveis ​​externas (por exemplo, dados meteorológicos ou econômicos) e variáveis ​​sazonais.

Configuração de parâmetros: o XGBoost tem muitos parâmetros de configuração que podem afetar a precisão do modelo. Isso inclui parâmetros de regularização, parâmetros de aprendizado e parâmetros relacionados ao número de árvores a serem usadas no modelo.

Treinamento do modelo: depois de preparar os dados e escolher as variáveis, é hora de treinar o modelo. Um método comum para treinar um modelo XGBoost para séries temporais é usar o recurso rolling forecast, onde o modelo é atualizado a cada nova observação de dados.

Avaliação do modelo: finalmente, é importante avaliar o modelo para verificar sua precisão e detectar possíveis problemas. Isso pode incluir a comparação dos resultados previstos com os resultados observados e o uso de técnicas de validação cruzada para testar a robustez do modelo.

Em geral, o modelo XGBoost pode ser uma ferramenta muito poderosa para previsão de séries temporais. No entanto, é importante ter em mente que a construção de um modelo preciso requer um processo cuidadoso de seleção de variáveis, configuração de parâmetros e treinamento do modelo."
        ),
        tabPanel(
          "Arima",
          "O modelo ARIMA (Autoregressive Integrated Moving Average) é uma técnica de análise de séries temporais que combina a regressão linear com a análise de resíduos. Ele é utilizado para modelar e prever valores futuros de séries temporais, levando em conta o comportamento passado e presente da série.

O modelo ARIMA é composto por três componentes principais: o componente autoregressivo (AR), o componente de média móvel (MA) e o componente de diferenciação (I).

O componente AR é responsável por modelar o comportamento de autocorrelação na série temporal. Ele utiliza valores passados da série para prever valores futuros. O componente MA é responsável por modelar o comportamento de médias móveis na série temporal. Ele utiliza os erros passados da previsão da série para prever valores futuros.

O componente I é responsável por diferenciar a série temporal para torná-la estacionária. Caso a série não seja estacionária, a diferenciação é necessária para eliminar tendências e variações sazonais na série.

O modelo ARIMA utiliza técnicas estatísticas para determinar os parâmetros ótimos para cada componente, com o objetivo de minimizar o erro geral do modelo. O modelo é então utilizado para prever valores futuros da série temporal."
        )
      )
    )
))


########################################

shinyApp(
  ui = dashboardPage(
    preloader = list(html = tagList(spin_1(), "Thalis.R: Carregando"), color = "#F14902"),
    help = TRUE,
    scrollToTop = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = sprintf("Ceasa - ce", as.character(utils::packageVersion("bs4Dash"))),
        color = "primary",
        href = "",
        image = "https://play-lh.googleusercontent.com/kFwgSRrveDZRgkGWmItfpADxlPoQeRCnhSLqHBE8sT71UoMOqdzchTcF-dHzsOwnzK0",
        opacity = 0.95
      ),
      fixed = T,
      rightUi = tagList(
        dropdownMenu(
          badgeStatus = "danger",
          type = "messages",
          ######## Messagem item ##############
          messageItem(
            inputId = "triggerAction1",
            message = "Dados atualizados até o fim de Março",
            from = "Thalis Rebouças",
            image = "https://thalisreboucas.com.br/images/all/1.jpg",
            time = "31/03/2023",
            color = "orange"
          )
          ###################################
        ),
        userOutput("user")
      ),
      leftUi = tagList(
        dropdownMenu(
          badgeStatus = "info",
          type = "notifications",
          ###### Notification item ##########
          notificationItem(
            inputId = "triggerAction2",
            text = "Dados de 31/03/2023 ",
            status = "success"
          )
          ################################
        ),
      )
    ),
    sidebar = dashboardSidebar(
      fixed = T,
      collapsed = T,
      expandOnHover = T,
      skin = "light",
      status = "primary",
      id = "sidebar",
      customArea = fluidRow(
      ),
      sidebarUserPanel(
        image = "logo.png",
        name = "Thalis.R!"
      ),
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        sidebarHeader("Tabelas"),
        menuItem(
          "Tabela de Preço Atual",
          tabName = "tab_prices",
          icon = icon("th")
        ),
        menuItem(
          "Análise de dados",
          tabName = "analises_tab",
          icon = icon("chart-bar")
        ),
        
        sidebarHeader("Modelos"),
        menuItem(
          "Modelos de Previsão",
          tabName = "forecast_tab",
          icon = icon("chart-line")
        ),
        menuItem(
          "Teoria dos Modelos",
          tabName = "theory_tab",
          icon = icon("layer-group")
          )
        )
      ),
        
    body = dashboardBody(
      use_theme(mytheme),
      tabItems(
        actual_price_tab,
        analises_tab,
        forcast_model_tab,
        theory_tab
      )
    ),
    footer = dashboardFooter(
      fixed = FALSE,
      right = "© THALIS REBOUÇAS 2023 / TODOS OS DIREITOS RESERVADOS."
    ),
    title = "Ceasa dash"
  ),
  
  ################################ server ##################
  server = function(input, output, session) {
    useAutoColor(T)
    item <- reactive(as.numeric({input$item}))
    year_violin <- reactive(as.numeric({input$year_violin}))
    # alerts ------------------------------------------------------------------
    observeEvent(input$show_alert, {
      print("created")
      createAlert(
        id = "alert_anchor",
        options = list(
          title = "Be Careful!",
          status = "danger",
          closable = TRUE,
          width = 12,
          content = "Danger alert preview. This alert is dismissable. 
          A wonderful serenity has taken possession of my entire soul, 
          like these sweet mornings of spring which 
          I enjoy with my whole heart."
        )
      )
    })
    
    observeEvent(input$hide_alert, {
      print("deleted")
      closeAlert(id = "alert_anchor")
    })
    
    # alert callback event
    observeEvent(input$alert_anchor, {
      alertStatus <- if (input$alert_anchor) "opened" else "closed"
      toastColor <- if (input$alert_anchor) "bg-lime" else "bg-fuchsia"
      toast(
        title = sprintf("Alert succesfully %s!", alertStatus),
        options = list(
          class = toastColor,
          autohide = TRUE,
          position = "bottomRight"
        )
      )
    })
    
    
    # plots -------------------------------------------------------------------
    
    output$plot_eda <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_anomaly_diagnostics(.value = value,.date_var = date) %>% plot_ly() %>%
        add_lines(x = ~date,
                  y = ~observed,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Dados") %>%
        add_lines(x = ~date , y = ~trend , name ="tendência" ,
                  line = list(color = "rgb(4, 86, 74)")) %>% 
        layout(showlegend = T,
               title='',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = "Dia",
                            gridcolor = 'ffff',
                            rangeslider = list(visible = T)))
      
      })
    
    output$plot_an <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_anomaly_diagnostics(.value = value,.date_var = date) %>% plot_ly() %>%
        add_lines(x = ~date,
                  y = ~observed,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Dados") %>%
        add_ribbons(x = ~date,
                    ymin = ~recomposed_l1,
                    ymax = ~recomposed_l2,
                    line = list(color = "rgba(105, 172, 135 ,0.5)"),
                    fillcolor = "rgba(105, 172, 135, 0.3)",
                    name = "Fator IQR") %>% 
        add_markers(x = ~date,
                    y = ~observed,
                    marker = list(color = "#ff0000"),
                    data =  data %>%
                      dplyr::group_by(id) %>%
                      dplyr::filter(id == item()) %>%  
                      tk_anomaly_diagnostics(.value = value,.date_var = date) %>% 
                      dplyr::filter(anomaly == "Yes") ,
                    name = 'Anomalia')%>%
        layout(showlegend = T,
               title='',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = "Dia",
                            gridcolor = 'ffff',
                            rangeslider = list(visible = T)))
      
      
    })

    
    
    output$plot_violin <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
        dplyr::filter(year == year_violin()) %>% 
        plot_ly(
          y = ~.value,
          x= ~year,
          type = 'violin',
          color = I("rgba(105, 172, 135 ,0.8)"),
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
        ) %>% 
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_violin_month <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
        dplyr::filter(year == year_violin()) %>% 
        plot_ly(
          y = ~.value,
          x= ~month.lbl,
          type = 'violin',
          color = I("rgba(105, 172, 135 ,0.8)"),
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
        ) %>% 
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_violin_week <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
        dplyr::filter(year == year_violin()) %>% 
        plot_ly(
          y = ~.value,
          x= ~wday.lbl,
          type = 'violin',
          color = I("rgba(105, 172, 135 ,0.8)"),
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
        ) %>% 
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_violin_weeks <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
        dplyr::filter(year == year_violin()) %>% 
        plot_ly(
          y = ~.value,
          x= ~week,
          type = 'violin',
          color = I("rgba(105, 172, 135 ,0.8)"),
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
        ) %>% 
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    
    output$plot_violin_quarter <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
        dplyr::filter(year == year_violin()) %>% 
        plot_ly(
          y = ~.value,
          x= ~quarter,
          type = 'violin',
          color = I("rgba(105, 172, 135 ,0.8)"),
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
        ) %>% 
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_stl <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_anomaly_diagnostics(.value = value,.date_var = date) %>% plot_ly() %>%
        add_lines(x = ~date,
                  y = ~remainder,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Restante") %>%
        layout(showlegend = T,
               title='',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = "Ano",
                            gridcolor = 'ffff'))
    })
    
    
    output$plot_acf <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_acf_diagnostics(.date_var = date,.value = value) %>% 
        plot_ly() %>%
        add_lines(x = ~lag,
                  y = ~ACF,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Correlação ACF") %>%
        add_lines(y = ~.white_noise_lower,
                  x = ~lag,
                  line = list(color = "rgb(0, 0, 0)",dash = 'dash'),
                  name = "Ruído Branco inferior ") %>%
        add_lines(y = ~.white_noise_upper,
                  x = ~lag,
                  line = list(color = "rgba(0, 0, 0 )",dash = 'dash'),
                  name = "Ruído Branco superior") %>% 
        layout(
          yaxis = list(title = "Correlação ACF" ),
          xaxis = list(title = "Quantidade de Lags"))
        
    })
    
    
    output$plot_pacf <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_acf_diagnostics(.date_var = date,.value = value) %>% 
        plot_ly() %>%
        add_lines(x = ~lag,
                  y = ~PACF,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Correlação PACF") %>%
        add_markers(x = ~lag,
                    y = ~PACF,
                    marker = list(color = "rgb(105, 175, 94)",
                                  size = 4),
                    name = "Pontos da Correlação ACF") %>% 
        add_lines(y = ~.white_noise_lower,
                  x = ~lag,
                  line = list(color = "rgb(0, 0, 0)",dash = 'dash'),
                  name = "Ruído Branco inferior ") %>%
        add_lines(y = ~.white_noise_upper,
                  x = ~lag,
                  line = list(color = "rgba(0, 0, 0 )",dash = 'dash'),
                  name = "Ruído Branco superior") %>% 
        layout(
          yaxis = list(title = "Correlação PACF" ),
          xaxis = list(title = "Quantidade de Lags"))
      
    })
    
    
    
    ##### tab 1 ---- actual price ###################
    output$tbl <- DT::renderDataTable({
      data %>% dplyr::mutate(my = zoo::as.yearmon(date)) %>% 
        dplyr::filter( date >= input$dateRange[2] &  date <= input$dateRange[1] ) %>% 
        dplyr::mutate(Produto = paste(produte,unit,sep = " ")) %>% 
        dplyr::group_by(my,Produto) %>% 
          dplyr::summarise(value = round(mean(value),2)) %>% drop_na() %>%
          dplyr::arrange(desc(my)) %>%  
            tidyr::pivot_wider(id_cols = "Produto", names_from = my, 
                           values_from = value)  %>% 
                             DT::datatable(filter = 'top', 
                                           options = list(
                                           language = pt,
                                           mark = ",",
                                           pageLength = 10, 
                                           autoWidth = TRUE,
                                           scrollX = TRUE)) 
      })
    
    ###### tab de explorator data --- ########################
    
    output$tbl_eda_1 <- renderTable({ data %>%
                                       dplyr::group_by(id) %>%
                                       dplyr::filter(id == item()) %>% 
                                       timetk::tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
                                       dplyr::group_by(year) %>% 
                                       dplyr::filter(year == year_violin()) %>%
                                       dplyr::summarize( Mínimo = min(.value),
                                                         Média = mean(.value),`Desvio Padrão` = sd(.value) , 
                                                         IQR = IQR(.value) , 
                                                         Curtose = as.numeric(kurtosis(.value)) , 
                                                         Assimetria = as.numeric(skewness(.value)),
                                                         Q1 = stats::quantile(.value,probs = 0.25, na.rm = TRUE), 
                                                         Mediana = median(.value),
                                                         Q3 = stats::quantile(.value,probs = 0.75, na.rm = TRUE),
                                                         Maxímo = max(.value),
                                                         Amplitude = (max(.value) -min(.value))) 
                                     
    })
    
   output$tbl_1 <- DT::renderDataTable({
   data %>% dplyr::group_by(produte) %>% 
     dplyr::filter(date >= input$dateRange[2] & date <= input$dateRange[1] ) %>% 
      dplyr::mutate(Produto = paste(produte,unit,sep = " ")) %>% 
   dplyr::arrange(desc(date)) %>%  
     tidyr::pivot_wider(id_cols = "Produto", names_from = date, 
                        values_from = value)    %>%  
     DT::datatable(filter = 'top', options = list(
       language = pt,
       pageLength = 11, autoWidth = TRUE,scrollX = TRUE))
   })
   
    # card API ----------------------------------------------------------------
    
    
    observeEvent(input$triggerCard, {
      updateBox(id = "mycard", action = input$cardAction)
    })
    
    observeEvent(input$update_box, {
      updateBox(
        "mycard",
        action = "update",
        options = list(
          title = h3(class = "card-title", "hello", dashboardBadge(1, color = "primary")),
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          background = NULL,
          height = "900px",
          closable = FALSE
        )
      )
    })
    
    observe({
      print(
        list(
          collapsed = input$mycard$collapsed,
          maximized = input$mycard$maximized,
          visible = input$mycard$visible
        )
      )
    })
    
    
    # card sidebar API --------------------------------------------------------
    
    observeEvent(input$toggle_card_sidebar, {
      updateBoxSidebar("mycardsidebar")
    })
    
    observeEvent(input$sidebar, {
      toastOpts$class <- if (input$sidebar) "bg-success" else "bg-danger"
      toast(
        title = if (input$sidebar) "Sidebar Está aberta!" else "Sidebar está fechada!",
        options = toastOpts
      )
    })


   
    
    
   
  }

)