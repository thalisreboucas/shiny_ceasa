
############
EDA <- function(product_id){ 
  dt <- data %>%
    dplyr::group_by(id) %>%
    dplyr::filter(id == product_id)
  
  graph <-  dt %>%
    timetk::plot_time_series( .date_var    = date,
                             .value       = value)
  
  table <- dt %>%  datawizard::describe_distribution()

  #### ESSE  
  tb <- dt %>%  dplyr::mutate(my = zoo::as.yearmon(date)) %>% 
    tidyr::pivot_wider(id_cols = "Produto", names_from = my, 
                       values_from = value ,values_fn = list) %>% describe_distribution()
  
  anomaly <- dt %>% plot_anomaly_diagnostics(date,
                                             value)
  

  acf <- dt %>% plot_acf_diagnostics(
    date, value,               # ACF & PACF
    .lags = "30 days",          # 7-Days of hourly lags
    .interactive = FALSE
  )
  
  seasonal <- dt %>% plot_seasonal_diagnostics(date, value, .interactive = T)
  
  
  stl <- dt %>% plot_stl_diagnostics(  date, value,
                                       .feature_set = c("observed","season","trend","remainder"),
                                       .interactive = T)
  
  return(list(
    table <- table,
    graph <- graph ,
    anomaly <- anomaly ,
    acf <- acf,
    seasonal <- seasonal,
    stl <- stl,
    tb_2 <- tb
  ))  
  
}

### grafico padrão 

data %>%
 filter(id ==1 ) %>% 
  tk_anomaly_diagnostics(.value = value,.date_var = date) %>% plot_ly() %>%
  add_lines(x = ~date,
            y = ~remainder,
            line = list(color = "rgb(105, 175, 94)"),
            name = "Dados") %>%
  layout(showlegend = T,
         title='',
         yaxis = list(title = "Preço",
                      tickprefix = "R$",
                      gridcolor = 'ffff'),
         xaxis = list(title = "Ano",
                      gridcolor = 'ffff'))

### grafico  de anomalia 

dt %>%  tk_anomaly_diagnostics(.value = value,.date_var = date) %>% plot_ly() %>%
  add_lines(x = ~date,
            y = ~observed,
            line = list(color = "rgb(105, 175, 94)"),
            name = "Dados") %>%
  add_ribbons(x = ~date,
              ymin = ~recomposed_l1,
              ymax = ~recomposed_l2,
              line = list(color = "rgba(105, 172, 135 ,0.5)"),
              fillcolor = "rgba(105, 172, 135, 0.3)",
              name = "Fator Iqr") %>% 
   add_markers(x = ~date,
               y = ~observed,
               marker = list(color = "#ff0000"),
               data = dt %>% 
                 tk_anomaly_diagnostics(.value = value,.date_var = date) %>% 
                 dplyr::filter(anomaly == "Yes") ,
               name = 'Anomalia')%>%
  layout(showlegend = T,
         title='Gráfico de Anomalia',
         yaxis = list(title = "Preço",
                      tickprefix = "R$",
                      gridcolor = 'ffff'),
         xaxis = list(title = "Dia",
                      gridcolor = 'ffff',
                      rangeslider = list(visible = T)))


### box plot
data %>% filter(id ==1 )%>% 
  plot_ly(
  y = ~value,
  type = 'violin',
  color = I("rgba(105, 172, 135 ,0.8)"),
  box = list(
    visible = T
  ),
  meanline = list(
    visible = T
  ),
  x0 = ' '
) %>% 
  layout(showlegend = F,
         title=' ',
         yaxis = list(title = "Preço",
                      tickprefix = "R$",
                      gridcolor = 'ffff'))

######## Finalizado.

  data %>% dplyr::mutate(my = zoo::as.yearmon(date)) %>% 
  dplyr::group_by(my,Produto) %>% 
  dplyr::summarise(value = round(mean(value),2)) %>% drop_na() %>% 
  dplyr::arrange(desc(my)) %>% 
  tidyr::pivot_wider(id_cols = "Produto", names_from = my, 
                     values_from = value)    %>% 
  datatable(filter = 'top', options = list(
                       pageLength = 10, autoWidth = TRUE))

  data %>%   dplyr::group_by(Produto) %>% 
    dplyr::filter(date >= "2023-03-02" & date <= last(date))
    dplyr::arrange(desc(date)) %>%  
    tidyr::pivot_wider(id_cols = "Produto", names_from = date, 
                       values_from = value)    %>%  
    datatable(filter = 'top', options = list(
                         pageLength = 10, autoWidth = TRUE))
   
    
    #### tabela de descrição  
    data %>%
      dplyr::group_by(id) %>%
      dplyr::filter(id == 1) %>% 
      dplyr::summarize( Mínimo = min(value),
                        Média = mean(value),`Desvio Padrão` = sd(value) , 
                        IQR = IQR(value) , 
                        Curtose = as.numeric(kurtosis(value)) , 
                        Assimetria = as.numeric(skewness(value)),
                        Q1 = stats::quantile(value,probs = 0.25, na.rm = TRUE), 
                        Mediana = median(value),
                        Q3 = stats::quantile(value,probs = 0.75, na.rm = TRUE),
                        Maxímo = max(value),
                        Amplitude = (max(value) -min(value))) %>% knitr::kable()
    
    
  ########################### acf #################  
  data %>%
    dplyr::group_by(id) %>%
    dplyr::filter(id == 1) %>% tk_acf_diagnostics(.date_var = date,.value = value) %>% 
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
    
    data %>%
      dplyr::group_by(id) %>%
      dplyr::filter(id == 1) %>% tk_acf_diagnostics(.date_var = date,.value = value,) %>% 
      plot_ly() %>%
      add_lines(x = ~lag,
                y = ~PACF,
                line = list(color = "rgb(105, 175, 94)"),
                name = "Correlação ACF") %>%
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
        yaxis = list(title = "Correlação ACF" ),
        xaxis = list(title = "Quantidade de Lags"))
    
    
    
    data %>%
      dplyr::group_by(id) %>%
      dplyr::filter(id == 1) %>% plot_acf_diagnostics(.date_var = date,.value = value)
    
    ##################################################
    
    ################ new_stl diag 
    
    data %>%
      dplyr::group_by(id) %>%
      dplyr::filter(id == 1) %>% tk_stl_diagnostics(.date_var = date,.value = value) %>% 
      plot_ly() %>%
      add_lines(x = ~date,
                y = ~season,
                line = list(color = "rgb(105, 175, 94)"),
                name = "Dados")
    
    ## observed ,season , trend , remainder , seasadj
    ######################################################
   ##### new box plot  
    data %>%
      dplyr::group_by(id) %>%
      dplyr::filter(id == 1) %>% tk_seasonal_diagnostics(.date_var = date,.value = value) %>% 
      dplyr::filter(year == 2022)  %>% 
        plotly::plot_ly(
        y = ~.value,
        x = ~ week,
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
    # wday.lbl,week,quarter,month.lbl,year


      data %>% dplyr::mutate(my = zoo::as.yearmon(date)) %>% 
        dplyr::mutate(Produto = paste(produte,unit)) %>% 
        dplyr::group_by(my,Produto) %>% View()

      data_prediction |> 
        dplyr::group_by(id,Mes,Name_models) |> 
        dplyr::filter(id == 1,.key=="prediction") |>  
        dplyr::mutate(Mes = case_when( Mes == 1 ~ "Janeiro",
                                       Mes == 2 ~ "Feveiro",
                                       Mes == 3 ~ "Março",
                                       Mes == 4 ~ "Abril",
                                       Mes == 5 ~ "Maio",
                                       Mes == 6 ~ "Junho",
                                       Mes == 7 ~ "Julho",
                                       Mes == 8 ~ "Agosto",
                                       Mes == 9 ~ "Setembro",
                                       Mes == 10 ~ "Outubro",
                                       Mes == 11 ~ "Novembro",
                                       Mes == 12 ~ "Dezembro"
                                       
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
   