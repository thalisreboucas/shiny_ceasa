data_traing <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  group_by(id) %>% 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
      .model_id == 1 ~ "Prophet XG 1",
      .model_id == 2 ~ "Prophet XG 2",
      .model_id == 3 ~ "Prophet XG 3",
      .model_id == 4 ~ "Arima XG 1",
      .model_id == 5 ~ "Arima XG 1",
      .model_id == 6 ~ "NNAR"
  ))

data_metrics <- nested_modeltime_tbl %>% 
  extract_nested_test_accuracy() %>%
  group_by(id) 


data_traing %>%
  plot_ly() %>%
add_lines( data = data_traing %>% filter(id == 1,.key == "actual"),
            x = ~.index,
            y = ~.value,
            line = list(color = "rgb(105, 175, 94)"),
            name = "Dados") %>%
  add_lines( data = data_traing %>% 
               group_by(.model_id) %>%
               filter(id == 1,.key == "prediction"),
             x = ~.index,
             y = ~.value,
             color = ~.model_desc,
             name = ~Name_models) %>%
 # add_ribbons(x ~.index,
 #             ymin = ~.conf_lo,
#              ymax = ~.conf_hi,
 #             color = ~Name_models) %>% 
  layout(showlegend = T,
         title='',
         yaxis = list(title = "Preço",
                      tickprefix = "R$",
                      gridcolor = 'ffff'),
         xaxis = list(title = "Dia",
                      gridcolor = 'ffff',
                      rangeslider = list(visible = T)))


### fazer tabela de metricas

## fazer grafico de previsão

## fazer residuos e graficos / testes

