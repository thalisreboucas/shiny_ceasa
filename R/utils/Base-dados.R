# Base preço dos alimentos Ceasa
Dados_Ceasa_Preco <- readxl::read_excel("E:/Thalis/MEU/Ceasa/shiny_ceasa/R/utils/Dados.xlsx", 
                                col_types = c("text", "text", "text", "numeric", "date"))


data <- Dados_Ceasa_Preco  %>% dplyr::select(id,produte,unit,date,value) %>%  tidyr::drop_na()

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


data_traing <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  group_by(id) %>% 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
                      .model_id == 1 ~ "Prophet XG 1",
                      .model_id == 2 ~ "Prophet XG 2",
                      .model_id == 3 ~ "Prophet XG 3",
                      .model_id == 4 ~ "Arima XG 1",
                      .model_id == 5 ~ "Arima XG 2",
                      .model_id == 6 ~ "NNAR"
                    ))

data_metrics <- nested_modeltime_tbl %>% 
  extract_nested_test_accuracy() %>%
  group_by(id) %>% 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
                      .model_id == 1 ~ "Prophet XG 1",
                      .model_id == 2 ~ "Prophet XG 2",
                      .model_id == 3 ~ "Prophet XG 3",
                      .model_id == 4 ~ "Arima XG 1",
                      .model_id == 5 ~ "Arima XG 2",
                      .model_id == 6 ~ "NNAR"
                    ))



min_date <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  filter(.key == "prediction") %>% select(-id) %>% 
  summarise(min_date = unique(min(.index)))

actual <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  filter(.index >= min_date , .key == "actual") %>% 
  select(-.model_id,-.model_desc,-.conf_lo,-.conf_hi) %>% view()

prediction <- nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  filter(.index >= min_date , .key == "prediction") %>% 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
                      .model_id == 1 ~ "Prophet XG 1",
                      .model_id == 2 ~ "Prophet XG 2",
                      .model_id == 3 ~ "Prophet XG 3",
                      .model_id == 4 ~ "Arima XG 1",
                      .model_id == 5 ~ "Arima XG 2",
                      .model_id == 6 ~ "NNAR"),
                    pred_value = .value) %>% 
  select(-.conf_lo,-.conf_hi,-.value) 

 resuadual <- inner_join(actual,prediction,by = c("id",".index"))  %>% group_by(id,.index,Name_models) %>%  summarise( residual = .value-pred_value)                     
 
           