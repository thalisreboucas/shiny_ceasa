pacman::p_load(shiny,shinydashboard,
               bs4Dash,bs4cards,waiter,fresh,
               thematic,shinythemes,DT,zoo,plotly, # Making dynamic graphs
               modeltime, # Times model classics 
               tidyverse, # All tidy for manipulation
               timetk, # Making a tables and graph's of timeseries
               lubridate, # Working if date
               easystats, # making a spells on the datas.
               imputeTS,
               readxl,
               recipes,
               workflows,
               parsnip,
               Metrics)

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
      choices = c(2015,2016,2017,2018,2019,2020,2021,2022,2023,2024),
      selected = 2024)
    ),
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
  tabName = "forecast_diag_tab",
  fluidRow(
    box(
      closable = FALSE, 
      width = 3,
      solidHeader = TRUE, 
      status = "navy",
      collapsible = TRUE,
      selectInput(
        inputId = "item_2",
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
          "VAGEM"=47))),
    box(
      title = "Tabela com as metricas dos modelos", 
      closable = FALSE, 
      width = 9,
      solidHeader = TRUE, 
      status = "navy",
      collapsible = TRUE,
      tableOutput("metrics_models")
    ),
    box(
      title = "Gráfico de Treino do modelo", 
      width = 12,
      status = "navy", 
      closable = FALSE,
      maximizable = TRUE, 
      solidHeader = TRUE,
      collapsible = TRUE,
      tabPanel("Gráfico de Treino",plotly::plotlyOutput("plot_training_model"))
    ),
    
    tabBox(
      title = "Graficos Ruído Branco", 
      closable = FALSE, 
      width = 12,
      status = "navy", 
      solidHeader = TRUE, 
      collapsible = FALSE,
      type = "tabs",
      selected = "Ruído Branco serie",
      tabPanel(
        "Ruído Branco serie",plotly::plotlyOutput("plot_rbs")
      ) ,
      tabPanel(
        "Ruído Branco boxplot",
        plotly::plotlyOutput("plot_rbb")
      ))
  
  )
)


# prediction_model_tab ############

forecast_tab <- tabItem(
  tabName = "4cast_tab",
  fluidRow(
    box(
      closable = FALSE, 
      width = 3,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
      selectInput(
        inputId = "item_3",
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
          "VAGEM"=47))),
    box(
      title = "Gráfico de Predição", 
      width = 12,
      status = "primary", 
      closable = FALSE,
      maximizable = TRUE, 
      solidHeader = TRUE,
      collapsible = TRUE,
      tabPanel("Gráfico de Predição",plotly::plotlyOutput("plot_prediction_model"))
    ),
    box(
      title = "Tabela com as medidas explorátorias", 
      closable = FALSE, 
      width = 12,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
      tableOutput("tbl_eda_pred"))
  )
)

# theory_tab ---- 
theory_tab <- tabItem(
  tabName = "theory_tab",
 "https://thalisreboucas.github.io/Estatistica_Resumida_Book/"
)


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
            message = "Dados atualizados até fevereiro de 2024",
            from = "Thalis Rebouças",
            image = "https://thalisreboucas.com.br/images/all/1.jpg",
            time = "13/02/2024",
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
            text = "Dados de 13/02/2024 ",
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
          "Diagnósticos dos Modelos",
          tabName = "forecast_diag_tab",
          icon = icon("stethoscope")
        ),
        menuItem(
          "Modelos de Previsão",
          tabName = "4cast_tab",
          icon = icon("chart-line")
        ),
        sidebarHeader("Teoria"),
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
        forecast_tab,
        theory_tab
      )
    ),
    footer = dashboardFooter(
      fixed = FALSE,
      right = "© THALIS REBOUÇAS 2024 / TODOS OS DIREITOS RESERVADOS."
    ),
    title = "Ceasa dash"
  ),
  
  ################################ server ##################
  server = function(input, output, session) {
    useAutoColor(T)
    item <- reactive(as.numeric({input$item}))
    item_2 <- reactive(as.numeric({input$item_2}))
    item_3 <- reactive(as.numeric({input$item_3}))
    year_violin <- reactive(as.numeric({input$year_violin}))
  
    
    # plots -------------------------------------------------------------------
    
    output$plot_eda <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_anomaly_diagnostics(.value = value,.date_var = date) |>  plot_ly() |> 
        add_lines(x = ~date,
                  y = ~observed,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Dados") |> 
        add_lines(x = ~date , y = ~trend , name ="tendência" ,
                  line = list(color = "rgb(4, 86, 74)")) |>  
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
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_anomaly_diagnostics(.value = value,.date_var = date) |>  plot_ly() |> 
        add_lines(x = ~date,
                  y = ~observed,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Dados") |> 
        add_ribbons(x = ~date,
                    ymin = ~recomposed_l1,
                    ymax = ~recomposed_l2,
                    line = list(color = "rgba(105, 172, 135 ,0.5)"),
                    fillcolor = "rgba(105, 172, 135, 0.3)",
                    name = "Fator IQR") |>  
        add_markers(x = ~date,
                    y = ~observed,
                    marker = list(color = "#ff0000"),
                    data =  data |> 
                      dplyr::group_by(id) |> 
                      dplyr::filter(id == item()) |>   
                      tk_anomaly_diagnostics(.value = value,.date_var = date) |>  
                      dplyr::filter(anomaly == "Yes") ,
                    name = 'Anomalia')|> 
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
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_seasonal_diagnostics(.date_var = date,.value = value) |>  
        dplyr::filter(year == year_violin()) |>  
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
        ) |>  
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_violin_month <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_seasonal_diagnostics(.date_var = date,.value = value) |>  
        dplyr::filter(year == year_violin()) |>  
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
        ) |>  
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_violin_week <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_seasonal_diagnostics(.date_var = date,.value = value) |>  
        dplyr::filter(year == year_violin()) |>  
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
        ) |>  
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_violin_weeks <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_seasonal_diagnostics(.date_var = date,.value = value) |>  
        dplyr::filter(year == year_violin()) |>  
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
        ) |>  
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    
    output$plot_violin_quarter <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_seasonal_diagnostics(.date_var = date,.value = value) |>  
        dplyr::filter(year == year_violin()) |>  
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
        ) |>  
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_stl <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_anomaly_diagnostics(.value = value,.date_var = date) |>  plot_ly() |> 
        add_lines(x = ~date,
                  y = ~remainder,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Restante") |> 
        layout(showlegend = T,
               title='',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = "Ano",
                            gridcolor = 'ffff'))
    })
    
    
    output$plot_acf <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_acf_diagnostics(.date_var = date,.value = value) |>  
        plot_ly() |> 
        add_lines(x = ~lag,
                  y = ~ACF,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Correlação ACF") |> 
        add_lines(y = ~.white_noise_lower,
                  x = ~lag,
                  line = list(color = "rgb(0, 0, 0)",dash = 'dash'),
                  name = "Ruído Branco inferior ") |> 
        add_lines(y = ~.white_noise_upper,
                  x = ~lag,
                  line = list(color = "rgba(0, 0, 0 )",dash = 'dash'),
                  name = "Ruído Branco superior") |>  
        layout(
          yaxis = list(title = "Correlação ACF" ),
          xaxis = list(title = "Quantidade de Lags"))
        
    })
    
    
    output$plot_pacf <-  plotly::renderPlotly({
      data |> 
        dplyr::group_by(id) |> 
        dplyr::filter(id == item()) |>  
        tk_acf_diagnostics(.date_var = date,.value = value) |>  
        plot_ly() |> 
        add_lines(x = ~lag,
                  y = ~PACF,
                  line = list(color = "rgb(105, 175, 94)"),
                  name = "Correlação PACF") |> 
        add_markers(x = ~lag,
                    y = ~PACF,
                    marker = list(color = "rgb(105, 175, 94)",
                                  size = 4),
                    name = "Pontos da Correlação ACF") |>  
        add_lines(y = ~.white_noise_lower,
                  x = ~lag,
                  line = list(color = "rgb(0, 0, 0)",dash = 'dash'),
                  name = "Ruído Branco inferior ") |> 
        add_lines(y = ~.white_noise_upper,
                  x = ~lag,
                  line = list(color = "rgba(0, 0, 0 )",dash = 'dash'),
                  name = "Ruído Branco superior") |>  
        layout(
          yaxis = list(title = "Correlação PACF" ),
          xaxis = list(title = "Quantidade de Lags"))
      
    })
    
    ############################# forecast model ################
    output$plot_training_model <- plotly::renderPlotly({
      data_traing |> 
      plot_ly() |> 
      add_lines( data = data_traing  |>  filter(id == item_2(),
                                                .key == "actual",
                                                .index > min(data_traing  |> 
                                                filter(.key == "prediction") |>
                                                pull(.index))),
                 x = ~.index,
                 y = ~.value,
                 line = list(color = "rgb(105, 175, 94)"),
                 name = "Dados") |> 
      add_lines( data = data_traing  |>  
                   group_by(.model_id) |> 
                   filter(id == item_2(),.key == "prediction"),
                 x = ~.index,
                 y = ~.value,
                 color = ~.model_desc,
                 name = ~Name_models) |> 
      layout(showlegend = T,
             title='',
             yaxis = list(title = "Preço",
                          tickprefix = "R$",
                          gridcolor = 'ffff'),
             xaxis = list(title = "Dia",
                          gridcolor = 'ffff',
                          rangeslider = list(visible = T)))
    
    })
    
    
    output$plot_rbs <-  plotly::renderPlotly({
      residual |>  filter(id == item_2()) |>  
        ungroup() |>  
        group_by(Name_models) |>  
        plot_ly(
          x= ~.index,
          y = ~residual,
          color = ~Name_models,
          name = ~Name_models,
          type = 'scatter', mode = 'lines'
        ) |>  
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Resíduos",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    output$plot_rbb <-  plotly::renderPlotly({
      residual |>  filter(id == item_2()) |>  
        ungroup() |>  
        group_by(Name_models) |>  
        plot_ly(
          y = ~residual,
          type = 'violin',
          color = ~Name_models,
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
        ) |>  
        layout(showlegend = F,
               title=' ',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = " "))
    })
    
    
    
    
    
    output$plot_prediction_model <- plotly::renderPlotly({
      data_prediction |> 
        plot_ly() |> 
        add_lines( data = data_prediction |>  
                     dplyr::filter(id == item_3(),
                                   .key == "actual",
                                   Ano >= 2023),
                   x = ~.index,
                   y = ~.value,
                   line = list(color = "rgb(105, 175, 94)"),
                   name = "Dados") |> 
        add_lines( data = data_prediction  |>  
                     group_by(.model_id) |> 
                     filter(id == item_3(),.key == "prediction"),
                   x = ~.index,
                   y = ~.value,
                   color = ~.model_desc,
                   name = ~Name_models) |> 
        layout(showlegend = T,
               title='',
               yaxis = list(title = "Preço",
                            tickprefix = "R$",
                            gridcolor = 'ffff'),
               xaxis = list(title = "Dia",
                            gridcolor = 'ffff',
                            rangeslider = list(visible = T)))
      
    })
    
    
    ##### tab 1 ---- actual price ###################
    output$tbl <- DT::renderDataTable({
      data |>  dplyr::mutate(my = zoo::as.yearmon(date)) |>  
        dplyr::filter( date >= input$dateRange[2] &  date <= input$dateRange[1] ) |>  
        dplyr::mutate(Produto = paste(produte,unit,sep = " ")) |>  
        dplyr::group_by(my,Produto) |>  
          dplyr::summarise(value = round(mean(value),2)) |>  drop_na() |> 
          dplyr::arrange(desc(my)) |>   
            tidyr::pivot_wider(id_cols = "Produto", names_from = my, 
                           values_from = value)  |>  
                             DT::datatable(filter = 'top', 
                                           options = list(
                                           language = pt,
                                           mark = ",",
                                           pageLength = 10, 
                                           autoWidth = TRUE,
                                           scrollX = TRUE)) 
      })
    
    ###### tab de explorator data --- ########################
    
    output$tbl_eda_1 <- renderTable({ data |> 
                                       dplyr::group_by(id) |> 
                                       dplyr::filter(id == item()) |>  
                                       timetk::tk_seasonal_diagnostics(.date_var = date,.value = value) |>  
                                       dplyr::group_by(year) |>  
                                       dplyr::filter(year == year_violin()) |> 
                                       dplyr::rename(Ano = year) |> 
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
    
    
    
    output$tbl_eda_pred <- renderTable({ data_prediction |> 
        dplyr::group_by(id,Mes,Name_models) |> 
        dplyr::filter(id == item_3(),.key=="prediction") |>  
        dplyr::mutate(Mes = case_when( Mes == 1 ~ "Janeiro",
                                       Mes == 2 ~ "Fevereiro",
                                       Mes == 3 ~ "Março",
                                       Mes == 4 ~  "Abril",
                                       Mes == 5 ~  "Maio",
                                       Mes == 6 ~  "Junho",
                                       Mes == 7 ~  "Julho",
                                       Mes == 8 ~  "Agosto",
                                       Mes == 9 ~  "Setembro",
                                       Mes == 10 ~  "Outubro",
                                       Mes == 11 ~  "Novembro",
                                       Mes == 12 ~  "Dezembro"
                                       
                                       
        )) |> 
        dplyr::summarize( Mínimo = min(.value),
                          Média = mean(.value),`Desvio Padrão` = sd(.value) , 
                          IQR = IQR(.value) , 
                          Mad = mad(.value),
                          Curtose = as.numeric(kurtosis(.value)) , 
                          Assimetria = as.numeric(skewness(.value)),
                          Q1 = stats::quantile(.value,probs = 0.25, na.rm = TRUE), 
                          Mediana = median(.value),
                          Q3 = stats::quantile(.value,probs = 0.75, na.rm = TRUE),
                          Maxímo = max(.value),
                          Amplitude = (max(.value) -min(.value)),
                          Dias = length(.value)) 
      
    })
    
    
    output$metrics_models <- renderTable({
      residual |> dplyr::filter(id == item_2())|> 
        dplyr::ungroup() |> 
        dplyr::group_by(Name_models) |> 
        dplyr::rename(Modelo = Name_models) |> 
        dplyr::summarise( Mae = mae(.value,pred_value) ,
                          Mape = mape(.value,pred_value) ,
                          Mase = mase(.value,pred_value),
                          smape = smape(.value,pred_value),
                          rmse = rmse(.value,pred_value),
                          rsq = cor(.value,pred_value)^2 ,
                          Média = mean(residual), 
                          `Desvio Padrão` = sd(residual),
                          Mediana = median(residual),
                          `Teste Shapiro` = (if(shapiro.test(residual)$p.value > 0.05 )
                            "Normal"
                            else "Não Normal"),
                          `Teste Box-Pierce` = (if(stats::Box.test(residual)$p.value > 0.05 )
                            "Independente"
                            else "Dependente"))
      

    })
    
   output$tbl_1 <- DT::renderDataTable({
   data |>  dplyr::group_by(produte) |>  
     dplyr::filter(date >= input$dateRange[2] & date <= input$dateRange[1] ) |>  
      dplyr::mutate(Produto = paste(produte,unit,sep = " ")) |>  
   dplyr::arrange(desc(date)) |>   
     tidyr::pivot_wider(id_cols = "Produto", names_from = date, 
                        values_from = value)    |>   
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