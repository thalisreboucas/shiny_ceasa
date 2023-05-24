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
    tabBox(
      title = "Tabela de Preços da Ceasa", 
      closable = FALSE, 
      width = 12,
      status = "primary", 
      solidHeader = TRUE, 
      collapsible = FALSE,
      type = "tabs",
      selected = "Preço diário",
      sidebar = boxSidebar(
        startOpen = FALSE,
        id = "mycardsidebar",
        background = "#ffffff",
        dateRangeInput('dateRange',
                       label = 'Filtro de dados para os itens',
                       language = "portuguese",
                       min = min(data$date),
                       max = max(data$date),
                       start = max(data$date) , 
                       end = min(data$date) ,
                       format = "dd/mm/yyyy" ,
                       separator = "Até" )),
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
    box(
      title = "Tabela com medidas de escala e dispersão", 
      closable = FALSE, 
      width = 12,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
      tableOutput("tbl_eda_1")
    ),
    
    box(
      title = "Tabela com medidas de escala e dispersão", 
      closable = FALSE, 
      width = 8,
      solidHeader = TRUE, 
      status = "primary",
      collapsible = TRUE,
      plotly::plotlyOutput("plot_violin")),
    box(
      width = 4,
      radioButtons(
      inputId = "year_violin",
      label = "Escolha o ano:",
      choices = c(2015,2016,2017,2018,2019,2020,2021,2022,2023),
      selected = 2023),
      radioButtons(
        inputId = "model_violin",
        label = "Como você quer agrupar :",
        choices = list("Dia da semana " = wday.lbl,
                       "Semana" = week,
                       "Mês" =  month.lbl,
                       "Ano" = year ),
        selected = year)
  )),
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
      tabPanel("Grafico de Anomalia",plotly::plotlyOutput("plot_an"))
      
    )
  ),
  
  fluidRow(
    box(
      title = "Grafico S-diag", 
      width = 12,
      status = "primary", 
      closable = FALSE,
      maximizable = TRUE, 
      solidHeader = TRUE, 
      collapsible = TRUE,
      plotly::plotlyOutput("plot_sd")
    ),
    box(
      title = "Grafico ACF", 
      width = 12,
      status = "primary", 
      closable = FALSE,
      maximizable = TRUE, 
      solidHeader = TRUE, 
      collapsible = TRUE,
      plotly::plotlyOutput("plot_acf")
    ),
    box(
      title = "Grafico STL", 
      width = 12,
      status = "primary", 
      closable = FALSE,
      maximizable = TRUE, 
      solidHeader = TRUE, 
      collapsible = TRUE,
      plotly::plotlyOutput("plot_stl")
    ))

)


# forcast_model_tab ---- #########################
forcast_model_tab <- tabItem(
  tabName = "forecast_tab",
  fluidRow(
    userBox(
      title = userDescription(
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
        title = "User card type 1",
        subtitle = "a subtitle here"
      ),
      collapsible = FALSE,
      ribbon(
        text = "New user",
        color = "fuchsia"
      ),
      status = "purple",
      elevation = 4,
      "Any content here"
    ),
    userBox(
      title = userDescription(
        type = 2,
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
        title = "User card type 2",
        subtitle = "a subtitle here",
        imageElevation = 4
      ),
      status = "teal",
      background = "teal",
      maximizable = TRUE,
      gradient = TRUE,
      progressBar(
        value = 5,
        striped = FALSE,
        status = "info"
      ),
      progressBar(
        value = 20,
        striped = TRUE,
        status = "warning"
      )
    )
  ),
  fluidRow(
    socialBox(
      title = userBlock(
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
        title = "Social Box",
        subtitle = "example-01.05.2018"
      ),
      "Some text here!",
      attachmentBlock(
        image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg",
        title = "Test",
        href = "https://google.com",
        "This is the content"
      ),
      lapply(X = 1:10, FUN = function(i) {
        boxComment(
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
          title = paste("Comment", i),
          date = "01.05.2018",
          paste0("The ", i, "-th comment")
        )
      }),
      footer = "The footer here!"
    ),
    box(
      title = "Box with user comment",
      status = "primary",
      userPost(
        id = 1,
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
        author = "Jonathan Burke Jr.",
        description = "Shared publicly - 7:30 PM today",
        "Lorem ipsum represents a long-held tradition for designers, 
       typographers and the like. Some people hate it and argue for 
       its demise, but others ignore the hate as they create awesome 
       tools to help create filler text for everyone from bacon 
       lovers to Charlie Sheen fans.",
        userPostTagItems(
          userPostTagItem(dashboardBadge("item 1", color = "warning")),
          userPostTagItem(dashboardBadge("item 2", color = "danger"))
        )
      ),
      userPost(
        id = 2,
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
        author = "Adam Jones",
        description = "Shared publicly - 5 days ago",
        userPostMedia(image = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
        userPostTagItems(
          userPostTagItem(dashboardBadge("item 1", color = "info")),
          userPostTagItem(dashboardBadge("item 2", color = "danger"))
        )
      )
    )
  ),
  fluidRow(
    box(
      status = "primary",
      width = 3,
      solidHeader = TRUE,
      boxProfile(
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
        title = "Nina Mcintire",
        subtitle = "Software Engineer",
        bordered = TRUE,
        boxProfileItem(
          title = "Followers",
          description = 1322
        ),
        boxProfileItem(
          title = "Following",
          description = 543
        ),
        boxProfileItem(
          title = "Friends",
          description = 13287
        )
      )
    ),
    box(
      title = "Card with messages",
      width = 9,
      footer =  tagList(
        actionButton("remove_message", "Remove"),
        actionButton("add_message", "Add"),
        actionButton("update_message", "Update"),
        numericInput("index_message", "Message index:", 1, min = 1, max = 3)
      ),
      userMessages(
        width = 6,
        status = "danger",
        id = "message",
        userMessage(
          author = "Alexander Pierce",
          date = "20 Jan 2:00 pm",
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
          type = "received",
          "Is this template really for free? That's unbelievable!"
        ),
        userMessage(
          author = "Sarah Bullock",
          date = "23 Jan 2:05 pm",
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
          type = "sent",
          "You better believe it!"
        )
      )
    )
  )
)

# theory_tab ---- 
theory_tab <- tabItem(
  tabName = "theory_tab",
  fluidRow(
    column(
      width = 6,
      tabBox(
        title = "A card with tabs",
        elevation = 2,
        id = "tabcard1",
        width = 12,
        collapsible = FALSE, 
        closable = FALSE,
        type = "tabs",
        status = "primary",
        solidHeader = TRUE,
        selected = "Tab 2",
        tabPanel(
          "Tab 1",
          "A wonderful serenity has taken possession of my entire soul,
          like these sweet mornings of spring which I enjoy with my
          whole heart. I am alone, and feel the charm of existence in
          this spot, which was created for the bliss of souls like mine.
          I am so happy, my dear friend, so absorbed in the exquisite sense
          of mere tranquil existence, that I neglect my talents. I should be
          incapable of drawing a single stroke at the present moment; and yet
          I feel that I never was a greater artist than now"
        ),
        tabPanel(
          "Tab 2",
          "The European languages are members of the same family.
          Their separate existence is a myth. For science, music,
          sport, etc, Europe uses the same vocabulary. The languages
          only differ in their grammar, their pronunciation and their
          most common words. Everyone realizes why a new common
          language would be desirable: one could refuse to pay expensive
          translators. To achieve this, it would be necessary to have
          uniform grammar, pronunciation and more common words. If several
          languages coalesce, the grammar of the resulting language is
          more simple and regular than that of the individual languages."
        ),
        tabPanel(
          "Tab 3",
          "Lorem Ipsum is simply dummy text of the printing and
          typesetting industry. Lorem Ipsum has been the industry's
          standard dummy text ever since the 1500s, when an unknown
          printer took a galley of type and scrambled it to make a
          type specimen book. It has survived not only five centuries,
          but also the leap into electronic typesetting, remaining
          essentially unchanged. It was popularised in the 1960s with
          the release of Letraset sheets containing Lorem Ipsum passages,
          and more recently with desktop publishing software like Aldus
          PageMaker including versions of Lorem Ipsum."
        )
      )
    ),
    column(
      width = 6,
      actionButton("update_tabBox2", "Toggle maximize tabBox", class = "my-2"),
      tabBox(
        title = "Tabs on right!",
        side = "right",
        id = "tabcard2",
        type = "tabs",
        elevation = 2,
        width = 12,
        status = "warning",
        maximizable = TRUE,
        collapsible = TRUE, 
        closable = TRUE,
        selected = "Tab 6",
        tabPanel(
          "Tab 4",
          "A wonderful serenity has taken possession of my entire soul,
          like these sweet mornings of spring which I enjoy with my
          whole heart. I am alone, and feel the charm of existence in
          this spot, which was created for the bliss of souls like mine.
          I am so happy, my dear friend, so absorbed in the exquisite sense
          of mere tranquil existence, that I neglect my talents. I should be
          incapable of drawing a single stroke at the present moment; and yet
          I feel that I never was a greater artist than now"
        ),
        tabPanel(
          "Tab 5",
          "The European languages are members of the same family.
          Their separate existence is a myth. For science, music,
          sport, etc, Europe uses the same vocabulary. The languages
          only differ in their grammar, their pronunciation and their
          most common words. Everyone realizes why a new common
          language would be desirable: one could refuse to pay expensive
          translators. To achieve this, it would be necessary to have
          uniform grammar, pronunciation and more common words. If several
          languages coalesce, the grammar of the resulting language is
          more simple and regular than that of the individual languages."
        ),
        tabPanel(
          "Tab 6",
          "Lorem Ipsum is simply dummy text of the printing and
          typesetting industry. Lorem Ipsum has been the industry's
          standard dummy text ever since the 1500s, when an unknown
          printer took a galley of type and scrambled it to make a
          type specimen book. It has survived not only five centuries,
          but also the leap into electronic typesetting, remaining
          essentially unchanged. It was popularised in the 1960s with
          the release of Letraset sheets containing Lorem Ipsum passages,
          and more recently with desktop publishing software like Aldus
          PageMaker including versions of Lorem Ipsum."
        )
      )
    )
  ),
  br(), br(),
  fluidRow(
    # manually inserted panels
    column(
      width = 6,
      bs4Dash::tabsetPanel(
        id = "tabsetpanel1",
        selected = "Tab 2",
        tabPanel(
          "Tab 1",
          "Content 1"
        ),
        tabPanel(
          "Tab 2",
          "Content 2"
        ),
        tabPanel(
          "Tab 3",
          "Content 3"
        )
      )
    ),
    
    # programmatically inserted panels
    column(
      width = 6,
      bs4Dash::tabsetPanel(
        id = "tabsetpanel2",
        type = "pills",
        .list = lapply(1:3, function(i) {
          tabPanel(
            paste0("Tab", i),
            paste("Content", i)
          )
        })
      )
    )
  ),
  br(), br(),
  # Vertical panels: TO DO
  tabsetPanel(
    id = "tabsetpanel3",
    selected = "Tab 2",
    vertical = TRUE,
    tabPanel(
      "Tab 1",
      "Content 1"
    ),
    tabPanel(
      "Tab 2",
      "Content 2"
    ),
    tabPanel(
      "Tab 3",
      "Content 3"
    )
  )
)


########################################

shinyApp(
  ui = dashboardPage(
    preloader = list(html = tagList(spin_1(), "Thalis.R: Carregando"), color = "#F14902"),
    dark = FALSE,
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
      fixed = T,collapsed = T,
      expandOnHover = F,
      skin = "light",
      status = "primary",
      id = "sidebar",
      customArea = fluidRow(
      ),
      sidebarUserPanel(
        image = "https://thalisreboucas.com.br/images/logo.png",
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
    model_violin <- reactive({input$model_violin})
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

    output$plot_sd <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        plot_seasonal_diagnostics(date, value, .interactive = T)
    })
    
    
    
    output$plot_violin <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
        dplyr::filter(year == year_violin()) %>% 
        plot_ly(
          y = ~.value,
          x= ~model_violin(),
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
        plot_stl_diagnostics(  date, value,
                               .feature_set = c("observed","season","trend","remainder"),
                               .interactive = T)
    })
    
    
    output$plot_acf <-  plotly::renderPlotly({
      data %>%
        dplyr::group_by(id) %>%
        dplyr::filter(id == item()) %>% 
        plot_acf_diagnostics(
          date, value,               # ACF & PACF
          .lags = "30 days",          # 7-Days of hourly lags
          .interactive = FALSE
        )
    })
    
    
    
    ##### tab 1 ---- actual price ###################
    output$tbl <- DT::renderDataTable({
      data %>% dplyr::mutate(my = zoo::as.yearmon(date)) %>% 
        dplyr::filter( date >= input$dateRange[2] &  date <= input$dateRange[1] ) %>% 
        dplyr::group_by(my,Produto) %>% 
          dplyr::summarise(value = round(mean(value),2)) %>% drop_na() %>%
          dplyr::arrange(desc(my)) %>% 
            tidyr::pivot_wider(id_cols = "Produto", names_from = my, 
                           values_from = value)    %>%  
                             DT::datatable(filter = 'top', options = list(
                              pageLength = 10, autoWidth = TRUE,scrollX = TRUE)) 
      })
    
    ###### tab de explorator data --- ########################
    
    output$tbl_eda_1 <- renderTable({ data %>%
                                       dplyr::group_by(id) %>%
                                       dplyr::filter(id == item()) %>%  dplyr::summarize( Mínimo = min(value),
                                                                                          Média = mean(value),`Desvio Padrão` = sd(value) , 
                                                                                          IQR = IQR(value) , 
                                                                                          Curtose = as.numeric(kurtosis(value)) , 
                                                                                          Assimetria = as.numeric(skewness(value)),
                                                                                          Q1 = stats::quantile(value,probs = 0.25, na.rm = TRUE), 
                                                                                          Mediana = median(value),
                                                                                          Q3 = stats::quantile(value,probs = 0.75, na.rm = TRUE),
                                                                                          Maxímo = max(value),
                                                                                          Amplitude = (max(value) -min(value))) 
                                     
    })
    
   output$tbl_1 <- DT::renderDataTable({
   data %>% dplyr::group_by(Produto) %>% 
     dplyr::filter(date >= input$dateRange[2] & date <= input$dateRange[1] ) %>% 
   dplyr::arrange(desc(date)) %>%  
     tidyr::pivot_wider(id_cols = "Produto", names_from = date, 
                        values_from = value)    %>%  
     DT::datatable(filter = 'top', options = list(
       pageLength = 10, autoWidth = TRUE,scrollX = TRUE))
   })
   
    # card API ----------------------------------------------------------------
    
    output$cardAPIPlot <- renderPlot({
      if (input$mycard$maximized) {
        hist(rnorm(input$obsAPI))
      }
    })
    
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

    # tabBox API  -------------------------------------------------------------
    
    observeEvent(input$update_tabBox2, {
      updateBox("tabcard2_box", action = "toggleMaximize")
    })
    
    # update sidebar ----------------------------------------------------------
    
    observeEvent(input$sidebarToggle, {
      updateSidebar(id = "sidebar")
    })
    
    # user messages -----------------------------------------------------------
    
    observeEvent(input$remove_message, {
      updateUserMessages("message", action = "remove", index = input$index_message)
    })
    observeEvent(input$add_message, {
      updateUserMessages(
        "message",
        action = "add",
        content = list(
          author = "David",
          date = "Now",
          image = "https://i.pinimg.com/originals/f1/15/df/f115dfc9cab063597b1221d015996b39.jpg",
          type = "received",
          text = "Message content"
        )
      )
    })
    
    observeEvent(input$update_message, {
      updateUserMessages(
        "message",
        action = "update",
        index = input$index_message,
        content = list(
          text = tagList(
            appButton(
              inputId = "reload",
              label = "Click me!",
              icon = icon("arrows-rotate"),
              dashboardBadge(1, color = "primary")
            )
          )
        )
      )
    })
    
    observeEvent(input$reload, {
      showNotification("Yeah!", duration = 1, type = "default")
    })
    
    
    
    # user menu ---------------------------------------------------------------
   
  }

)