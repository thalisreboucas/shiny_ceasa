

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
  add_ribbons(x ~.index,
              ymin = ~.conf_lo,
              ymax = ~.conf_hi,
              line = list(color = 'transparent')) %>% 
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

