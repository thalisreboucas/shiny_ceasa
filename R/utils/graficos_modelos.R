data_traing <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
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
             line = list(color = ~.model_desc),
             name = "Dados") %>%
  
  layout(showlegend = T,
         title='',
         yaxis = list(title = "Preço",
                      tickprefix = "R$",
                      gridcolor = 'ffff'),
         xaxis = list(title = "Dia",
                      gridcolor = 'ffff',
                      rangeslider = list(visible = T)))
