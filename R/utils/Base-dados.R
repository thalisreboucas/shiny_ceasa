# Base preço dos alimentos Ceasa
Dados_Ceasa_Preco <- readxl::read_excel("E:/Thalis/MEU/shiny_ceasa/R/utils/Dados.xlsx", 
                                col_types = c("text", "text", "text", "numeric", "date"))


data <- Dados_Ceasa_Preco |> dplyr::select(id,produte,unit,date,value) |>  tidyr::drop_na()

remove(Dados_Ceasa_Preco)

### para a tabela do pacote DT
pt <- list(
    sEmptyTable = "Nenhum registro encontrado",
    sProcessing= "A processar...",
    sLengthMenu= "Mostrar _MENU_ registos",
    sZeroRecords= "Não foram encontrados resultados",
    sInfo= "Mostrando de _START_ até _END_ de _TOTAL_ registos",
    sInfoEmpty= "Mostrando de 0 até 0 de 0 registos",
    sInfoFiltered= "(filtrado de _MAX_ registos no total)",
    sInfoPostFix= "",
    sSearch= "Procurar:",
    sUrl= "",
    oPaginate= list(
      sFirst= "Primeiro",
      sPrevious= "Anterior",
      sNext= "Seguinte",
      sLast= "Último"
    ),
    oAria= list(
      sSortAscending= "= Ordenar colunas de forma ascendente",
      sSortDescending= "= Ordenar colunas de forma descendente"
    )
  )

nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  
  # Add workflows
  wflw_prophet,
  wflw_arima,
  wflw_nnetar
)


data_traing <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  group_by(id) %>% 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
                      .model_id == 1 ~ "Prophet XG",
                      .model_id == 2 ~ "Arima XG",
                      .model_id == 3 ~ "NNAR"
                    ))

data_metrics <- nested_modeltime_tbl %>% 
  extract_nested_test_accuracy() %>%
  group_by(id) %>% 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
                      .model_id == 1 ~ "Prophet XG 1",
                      .model_id == 2 ~ "Arima XG 1",
                      .model_id == 3 ~ "NNAR"
                    ))



min_date <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  filter(.key == "prediction") %>% select(-id) %>% 
  summarise(min_date = unique(min(.index)))

actual <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  dplyr::filter(.index >= min_date , .key == "actual") %>% 
  select(-.model_id,-.model_desc,-.conf_lo,-.conf_hi) 

prediction <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  filter(.index >= min_date , .key == "prediction") %>% 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
                      .model_id == 1 ~ "Prophet XG",
                      .model_id == 2 ~ "Arima XG",
                      .model_id == 3 ~ "NNAR"),
                    pred_value = .value) %>% 
  select(-.conf_lo,-.conf_hi,-.value) 

 residual <- dplyr::inner_join(actual,prediction,by = c("id",".index"))  |> 
              dplyr::group_by(id,.index,Name_models) |> 
              dplyr::summarise( residual = .value-pred_value)                      


###################### Prediction models #################

nested_modeltime_refit_tbl <-   modeltime_nested_refit(object = nested_modeltime_tbl,
                                                       control = control_nested_refit(verbose = TRUE)
)
